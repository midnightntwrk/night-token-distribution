use aiken/collection/dict.{to_pairs}
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId, to_dict}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use types.{
  FullUpgrade, NightProtocolParams, UpdateParameters, UpgradeParamsAction,
}
use utils.{intersect_length, master_token_name, must_find, sort_and_count}

validator protocol_params(one_shot: OutputReference) {
  spend(
    datum: Option<NightProtocolParams>,
    redeemer: UpgradeParamsAction,
    utxo: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      // (1) There will be an input which we call the protocol params input
      UpdateParameters -> {
        let Transaction { inputs, outputs, extra_signatories, .. } = tx

        // (1) There must be an input which we call the protocol params input.
        let own_input =
          must_find(inputs, fn(input) { input.output_reference == utxo })

        let Output { address, .. } = own_input.output

        // (1.1) The payment credential for the protocol params input is the
        //       executing script hash. This is enforced by the ledger.
        expect Script(own_script_hash) = address.payment_credential

        // (1.2) There protocol params input must contain a datum.
        expect Some(datum) = datum

        // (1.2.1) The datum must be of type `NightProtocolParams`.
        // (enforced by the datum typing above)
        let NightProtocolParams { tge_agent_auth_keys, min_auth_signatures, .. } =
          datum

        // (2) There must be an input which we call the protocol params output.
        // (2.2) The protocol params output value must only contain ADA
        //       and the previously minted Protocol Params NFT where the policy id is
        //       equal to the executing script's hash. The presence of ADA is enforced
        //       by the ledger.
        let param_output =
          must_find(
            outputs,
            fn(output) {
              when output.value |> to_dict |> to_pairs is {
                [_, output_policy] -> output_policy.1st == own_script_hash
                _ -> False
              }
            },
          )

        let Output { address: output_address, datum: output_datum, .. } =
          param_output

        // (2.3) The protocol params output must contain an `InlineDatum`.
        expect InlineDatum(output_datum) = output_datum

        // (2.3.1) The datum must be of type `NightProtocolParams`.
        expect NightProtocolParams {
          tge_agent_auth_keys: out_keys,
          min_auth_signatures: out_min,
          ..
        } = output_datum

        and {
          // (2.1) The protocol params output address's payment credential must equal
          //       the protocol params input address's payment credential
          output_address.payment_credential == address.payment_credential,
          // (2.3.2) The `NightProtocolParams` field `min_auth_signature`
          //         must be greater than `0`.
          out_min > 0,
          // (2.3.3) The `NightProtocolParams` field `tge_agent_auth_keys`
          //         must be a sorted and unique list of ByteArrays which
          //         are of 28 bytes in length. These are pubkey hashes which
          //         are 28 bytes in length.
          sort_and_count(out_keys) >= out_min,
          // (1.2.2) The number of the `NightProtocolParams` field `tge_agent_auth_keys`
          //         that match an existing signatory from the `Transaction` field
          //         `extra_signatories` must be greater than or equal to the
          //         `NightProtocolParams` field `min_auth_signature`.
          min_auth_signatures <= intersect_length(
            extra_signatories,
            tge_agent_auth_keys,
          ),
        }
      }

      // (1) There will be an input which we call the protocol params input.
      FullUpgrade -> {
        // (1.1) There protocol params input must contain a datum.
        expect Some(datum) = datum

        // (1.1.1) The datum must be of type `NightProtocolParams`.
        // (enforced by the datum typing above)
        let NightProtocolParams { tge_agent_auth_keys, min_auth_signatures, .. } =
          datum

        // (1.1.2) The number of the `NightProtocolParams` field `tge_agent_auth_keys`
        //         that match an existing signatory from the `Transaction` field
        //         `extra_signatories` must be greater than or equal to the
        //         `NightProtocolParams` field `min_auth_signature`.
        min_auth_signatures <= intersect_length(
          tx.extra_signatories,
          tge_agent_auth_keys,
        )
      }
    }
  }

  mint(_redeemer: Data, own_policy_id: PolicyId, tx: Transaction) {
    let Transaction { inputs, mint, outputs, .. } = tx

    // (1) check that the parameterized output reference
    //     is found in one of the inputs.
    expect _ =
      must_find(inputs, fn(input) { input.output_reference == one_shot })

    // (2) There must be exactly one minted token.
    //   (2.1) The policy_id must match the scripts own policy id.
    //         The own policy id is passed in by the ledger.
    //   (2.2) The token name must be `"NightProtocolParams"`.
    //   (2.3) The amount must be `1`.
    expect assets.from_asset(own_policy_id, master_token_name, 1) == mint

    // (3) There must be an output which we call the protocol params output.
    //
    // (3.2) The protocol params output value must only contain ADA
    //       and the minted value from above. The presence of ADA is enforeced
    //       by the ledger.
    let param_output =
      must_find(
        outputs,
        fn(output) {
          when output.value |> to_dict |> to_pairs is {
            [_, output_policy] -> output_policy.1st == own_policy_id
            _ -> False
          }
        },
      )

    let Output { address: output_address, datum: output_datum, .. } =
      param_output

    // (3.3) The protocol params output must contain an `InlineDatum`.
    expect InlineDatum(output_datum) = output_datum

    // (3.3.1) The inline datum must be of type `NightProtocolParams`.
    expect NightProtocolParams {
      tge_agent_auth_keys: out_keys,
      min_auth_signatures: out_min,
      ..
    } = output_datum

    and {
      // (3.3.2) The `NightProtocolParams` field `min_auth_signature`
      //         must be greater than `0`.
      out_min > 0,
      // (3.3.3) The `NightProtocolParams` field `tge_agent_auth_keys`
      //         must be a sorted and unique list of ByteArrays which
      //         are of 28 bytes in length. These are pubkey hashes which
      //         are 28 bytes in length.
      sort_and_count(out_keys) >= out_min,
      // (3.1) The protocol params output payment credential
      //       must match the script itself.
      output_address.payment_credential == Script(own_policy_id),
    }
  }

  // Catch all for all other purposes which are not supported
  // by this validator
  else(_) {
    fail
  }
}

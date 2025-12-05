use aiken/collection/dict.{to_pairs}
use cardano/assets.{PolicyId, to_dict}
use cardano/script_context.{ScriptContext}
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use types.{NightProtocolParams}
use utils.{intersect_length, must_find}

validator multisig(protocol_params_policy: PolicyId) {
  else(ctx: ScriptContext) {
    let Transaction { reference_inputs, extra_signatories, .. } =
      ctx.transaction

    // (1) There must be a reference input which we call the protocol params ref input.
    //   (1.1) The protocol params ref input value must only contain ADA
    //           and the previously minted Protocol Params NFT where the policy id is
    //           equal to the `protocol_params_policy` that parameterized the script.
    //           The presence of ADA is enforced by the ledger.
    let param_ref =
      must_find(
        reference_inputs,
        fn(input) {
          when input.output.value |> to_dict |> to_pairs is {
            [_, output_policy] -> output_policy.1st == protocol_params_policy
            _ -> False
          }
        },
      )

    // (1.2) The protocol params ref input must contain an `InlineDatum`.
    expect InlineDatum(param_datum) = param_ref.output.datum

    // (1.2.1) The datum must be of type `NightProtocolParams`.
    expect NightProtocolParams { tge_agent_auth_keys, min_auth_signatures, .. } =
      param_datum

    // (1.2.2) The number of the `NightProtocolParams` field `tge_agent_auth_keys`
    //         that match an existing signatory from the `Transaction` field
    //         `extra_signatories` must be greater than or equal to the
    //         `NightProtocolParams` field `min_auth_signature`.
    min_auth_signatures <= intersect_length(
      extra_signatories,
      tge_agent_auth_keys,
    )
  }
}

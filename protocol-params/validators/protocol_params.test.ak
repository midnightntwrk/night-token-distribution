use aiken/builtin
use aiken/collection/list.{concat}
use aiken/fuzz.{
  and_then, bool, bytearray_between, bytearray_fixed, constant, data,
  int_between, list_between, map, option,
}
use cardano/address.{Address, Script}
use cardano/assets.{Value, add, from_asset, from_lovelace, merge, zero}
use cardano/fuzz.{address_with, stake_credential} as f
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction, placeholder,
}
use config
use protocol_params
use types.{FullUpgrade, NightProtocolParams, UpdateParameters}
use utils.{master_token_name}

const hash = #"f94a7d9092d5115d23b2d0d9179578faaccbe634903b8ba15e6478f5"

const keys =
  [
    #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
    #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
  ]

const init =
  OutputReference(config.one_shot_tx_hash, config.one_shot_output_index)

const address = Address(Script(hash), None)

const empty_address = Address(Script(""), None)

const mint = from_asset(hash, master_token_name, 1)

const datum =
  NightProtocolParams(
    Script(""),
    #"",
    empty_address,
    empty_address,
    [],
    keys,
    1,
  )

const output =
  Output(
    address,
    from_lovelace(1500000) |> merge(mint),
    InlineDatum(datum),
    None,
  )

const empty_output = Output(address, from_lovelace(1500000), NoDatum, None)

const empty_input = Input(OutputReference(#"aa", 0), empty_output)

const out_ref = OutputReference(#"", 0)

const input = Input(out_ref, output)

const init_input = Input(init, empty_output)

const protocol_mint_transaction =
  Transaction {
    ..placeholder,
    inputs: [init_input],
    outputs: [output],
    mint: mint,
  }

const protocol_spend_upgrade_transaction =
  Transaction {
    ..placeholder,
    inputs: [input],
    outputs: [output],
    extra_signatories: keys,
  }

const protocol_spend_full_transaction =
  Transaction { ..placeholder, inputs: [input], extra_signatories: keys }

fn create_input(base: Input) -> Fuzzer<Input> {
  let output <- and_then(create_output(base.output))

  let input = Input { output_reference: out_ref, output }
  modify_output_reference(input)
}

fn create_output(base: Output) -> Fuzzer<Output> {
  let pick <- and_then(int_between(1, 8))

  when pick is {
    1 | 2 -> modify_address(base)
    3 | 4 -> modify_value(base)
    5 | 6 -> modify_datum(base)
    7 | 8 -> modify_all_output(base)
    _ -> fail
  }
}

fn modify_address(base: Output) -> Fuzzer<Output> {
  let byte_idx <- and_then(int_between(1, 220))

  let address <-
    map(
      address_with(
        constant(Script(builtin.rotate_bytearray(hash, byte_idx))),
        option(stake_credential()),
      ),
    )

  Output { ..base, address: address }
}

fn add_value(value: Value) -> Fuzzer<Value> {
  let policy <- and_then(bytearray_fixed(28))
  let token_name <- and_then(bytearray_between(0, 32))
  let amount <- map(int_between(1, 10))

  value |> add(policy, token_name, amount)
}

fn modify_value(base: Output) -> Fuzzer<Output> {
  let cond <- and_then(bool())

  let value <-
    map(
      if cond {
        let amount <- map(int_between(1000000, 2000000))

        from_lovelace(amount)
      } else {
        add_value(base.value)
      },
    )

  Output { ..base, value: value }
}

fn modify_datum(base: Output) -> Fuzzer<Output> {
  let data <- map(data())
  Output { ..base, datum: InlineDatum(data) }
}

fn modify_datum_keys(base: NightProtocolParams) -> Fuzzer<NightProtocolParams> {
  let new_key <- map(bytearray_fixed(28))
  NightProtocolParams {
    ..base,
    tge_agent_auth_keys: [new_key, ..base.tge_agent_auth_keys],
  }
}

fn modify_datum_min_sigs(
  base: NightProtocolParams,
) -> Fuzzer<NightProtocolParams> {
  let cond <- and_then(bool())

  if cond {
    constant(NightProtocolParams { ..base, min_auth_signatures: 0 })
  } else {
    let min <- map(int_between(4, 10))
    NightProtocolParams { ..base, min_auth_signatures: min }
  }
}

fn modify_output_reference(base: Input) -> Fuzzer<Input> {
  let output_reference <-
    map(f.output_reference_with(f.transaction_id(), constant(0)))

  Input { ..base, output_reference: output_reference }
}

fn modify_all_output(base: Output) -> Fuzzer<Output> {
  let next <- and_then(modify_address(base))
  let next <- and_then(modify_value(next))
  let final <- map(modify_datum(next))
  final
}

fn add_mint(value: Value) -> Fuzzer<Value> {
  let cond <- and_then(bool())
  let policy <- and_then(bytearray_fixed(28))
  let token_name <- and_then(bytearray_between(0, 32))
  let amount <- map(int_between(1, 10))

  if cond {
    value |> add(policy, token_name, amount)
  } else {
    value |> add(hash, token_name, amount)
  }
}

fn mints_between(base: Value, start: Int, last: Int) -> Fuzzer<Value> {
  let pick <- and_then(int_between(start, last))

  do_mints_between(pick, base)
}

fn do_mints_between(amount: Int, base: Value) -> Fuzzer<Value> {
  if amount == 0 {
    constant(base)
  } else {
    let mint <- and_then(add_mint(base))
    do_mints_between(amount - 1, mint)
  }
}

// ---------------------------------------------
// Mint Purpose
// --------------------------------------------

test param_1() {
  protocol_params.protocol_params.mint(init, 0, hash, protocol_mint_transaction)
}

test param_datum_keys(other_datum via modify_datum_keys(datum)) fail once {
  let output = Output { ..output, datum: InlineDatum(other_datum) }

  let tx = Transaction { ..protocol_mint_transaction, outputs: [output] }
  protocol_params.protocol_params.mint(init, 0, hash, tx)
}

test param_datum_min_sigs(other_datum via modify_datum_min_sigs(datum)) fail {
  let output = Output { ..output, datum: InlineDatum(other_datum) }

  let tx = Transaction { ..protocol_mint_transaction, outputs: [output] }
  protocol_params.protocol_params.mint(init, 0, hash, tx)
}

test param_no_outputs(
  other_outputs via list_between(create_output(empty_output), 0, 10),
) fail {
  let no_output =
    Transaction { ..protocol_mint_transaction, outputs: other_outputs }

  protocol_params.protocol_params.mint(init, 0, hash, no_output)
}

test param_no_inputs(
  other_inputs via list_between(create_input(empty_input), 0, 10),
) fail {
  let no_input =
    Transaction { ..protocol_mint_transaction, inputs: other_inputs }

  protocol_params.protocol_params.mint(init, 0, hash, no_input)
}

test param_no_nft_in_outputs() fail {
  let output = Output { ..output, value: from_lovelace(1500000) }
  let no_token = Transaction { ..protocol_mint_transaction, outputs: [output] }

  protocol_params.protocol_params.mint(init, 0, hash, no_token)
}

test param_extra_outputs(
  other_outputs via list_between(create_output(empty_output), 0, 10),
) {
  let more_outputs =
    Transaction {
      ..protocol_mint_transaction,
      outputs: concat(other_outputs, [output]),
    }
  protocol_params.protocol_params.mint(init, 0, hash, more_outputs)
}

test param_close_outputs(
  other_outputs via list_between(create_output(output), 1, 10),
) fail {
  let more_outputs =
    Transaction { ..protocol_mint_transaction, outputs: other_outputs }
  protocol_params.protocol_params.mint(init, 0, hash, more_outputs)
}

test param_no_mints(mint via mints_between(zero, 0, 10)) fail {
  let zero_mint = Transaction { ..protocol_mint_transaction, mint: mint }
  protocol_params.protocol_params.mint(init, 0, hash, zero_mint)
}

test param_extra_mints(mint via mints_between(mint, 1, 10)) fail {
  let more_mint = Transaction { ..protocol_mint_transaction, mint: mint }
  protocol_params.protocol_params.mint(init, 0, hash, more_mint)
}

test param_extra_inputs(
  other_inputs via list_between(create_input(empty_input), 0, 10),
) {
  let more_inputs =
    Transaction {
      ..protocol_mint_transaction,
      inputs: concat(other_inputs, [init_input]),
    }

  protocol_params.protocol_params.mint(init, 0, hash, more_inputs)
}

// ---------------------------------------------
// Spend Purpose - UpdateParameters
// --------------------------------------------

test spend_update_1() {
  protocol_params.protocol_params.spend(
    init,
    Some(datum),
    UpdateParameters,
    input.output_reference,
    protocol_spend_upgrade_transaction,
  )
}

test spend_update_no_datum() fail {
  protocol_params.protocol_params.spend(
    init,
    None,
    UpdateParameters,
    input.output_reference,
    protocol_spend_upgrade_transaction,
  )
}

test spend_update_input_datum_keys(
  other_datum via modify_datum_keys(datum),
) fail once {
  let input =
    Input(out_ref, Output { ..output, datum: InlineDatum(other_datum) })

  let tx = Transaction { ..protocol_spend_upgrade_transaction, inputs: [input] }
  protocol_params.protocol_params.spend(
    init,
    Some(other_datum),
    UpdateParameters,
    input.output_reference,
    tx,
  )
}

test spend_update_input_param_datum_min_sigs(
  other_datum via modify_datum_min_sigs(datum),
) fail once {
  let input =
    Input(out_ref, Output { ..output, datum: InlineDatum(other_datum) })

  let tx = Transaction { ..protocol_spend_upgrade_transaction, inputs: [input] }
  protocol_params.protocol_params.spend(
    init,
    Some(other_datum),
    UpdateParameters,
    input.output_reference,
    tx,
  )
}

test spend_update_no_inputs(
  other_inputs via list_between(create_input(empty_input), 0, 10),
) fail {
  let tx =
    Transaction { ..protocol_spend_upgrade_transaction, inputs: other_inputs }
  protocol_params.protocol_params.spend(
    init,
    Some(datum),
    UpdateParameters,
    input.output_reference,
    tx,
  )
}

test spend_update_close_inputs(
  other_inputs via list_between(create_input(input), 1, 10),
) fail {
  let tx =
    Transaction { ..protocol_spend_upgrade_transaction, inputs: other_inputs }
  protocol_params.protocol_params.spend(
    init,
    Some(datum),
    UpdateParameters,
    input.output_reference,
    tx,
  )
}

test spend_update_other_inputs(
  other_inputs via list_between(create_input(input), 0, 10),
) {
  let tx =
    Transaction {
      ..protocol_spend_upgrade_transaction,
      inputs: concat(other_inputs, [input]),
    }
  protocol_params.protocol_params.spend(
    init,
    Some(datum),
    UpdateParameters,
    input.output_reference,
    tx,
  )
}

test spend_update_no_outputs(
  other_outputs via list_between(create_output(empty_output), 0, 10),
) fail {
  let tx =
    Transaction { ..protocol_spend_upgrade_transaction, outputs: other_outputs }
  protocol_params.protocol_params.spend(
    init,
    Some(datum),
    UpdateParameters,
    input.output_reference,
    tx,
  )
}

test spend_update_close_outputs(
  other_outputs via list_between(create_output(output), 0, 10),
) fail {
  let tx =
    Transaction { ..protocol_spend_upgrade_transaction, outputs: other_outputs }
  protocol_params.protocol_params.spend(
    init,
    Some(datum),
    UpdateParameters,
    input.output_reference,
    tx,
  )
}

test spend_update_other_outputs(
  other_outputs via list_between(create_output(empty_output), 0, 10),
) {
  let tx =
    Transaction {
      ..protocol_spend_upgrade_transaction,
      outputs: concat(other_outputs, [output]),
    }
  protocol_params.protocol_params.spend(
    init,
    Some(datum),
    UpdateParameters,
    input.output_reference,
    tx,
  )
}

test spend_update_output_datum_keys(
  other_datum via modify_datum_keys(datum),
) fail once {
  let output = Output { ..output, datum: InlineDatum(other_datum) }

  let tx =
    Transaction { ..protocol_spend_upgrade_transaction, outputs: [output] }
  protocol_params.protocol_params.spend(
    init,
    Some(other_datum),
    UpdateParameters,
    input.output_reference,
    tx,
  )
}

test spend_update_output_param_datum_min_sigs(
  other_datum via modify_datum_min_sigs(datum),
) fail {
  let output = Output { ..output, datum: InlineDatum(other_datum) }

  let tx =
    Transaction { ..protocol_spend_upgrade_transaction, outputs: [output] }
  protocol_params.protocol_params.spend(
    init,
    Some(other_datum),
    UpdateParameters,
    input.output_reference,
    tx,
  )
}

// ---------------------------------------------
// Spend Purpose - FullUpgrade
// --------------------------------------------
test spend_full_1() {
  protocol_params.protocol_params.spend(
    init,
    Some(datum),
    FullUpgrade,
    input.output_reference,
    protocol_spend_full_transaction,
  )
}

test spend_full_no_datum() fail {
  protocol_params.protocol_params.spend(
    init,
    None,
    FullUpgrade,
    input.output_reference,
    protocol_spend_upgrade_transaction,
  )
}

test spend_full_input_datum_keys(
  other_datum via modify_datum_keys(datum),
) fail once {
  let input =
    Input(out_ref, Output { ..output, datum: InlineDatum(other_datum) })

  let tx = Transaction { ..protocol_spend_full_transaction, inputs: [input] }
  protocol_params.protocol_params.spend(
    init,
    Some(other_datum),
    FullUpgrade,
    input.output_reference,
    tx,
  )
}

test spend_full_input_param_datum_min_sigs(
  other_datum via modify_datum_min_sigs(datum),
) fail once {
  let input =
    Input(out_ref, Output { ..output, datum: InlineDatum(other_datum) })

  let tx = Transaction { ..protocol_spend_full_transaction, inputs: [input] }
  protocol_params.protocol_params.spend(
    init,
    Some(other_datum),
    FullUpgrade,
    input.output_reference,
    tx,
  )
}

test spend_full_other_inputs(
  other_inputs via list_between(create_input(input), 0, 10),
) {
  let tx =
    Transaction {
      ..protocol_spend_full_transaction,
      inputs: concat(other_inputs, [input]),
    }
  protocol_params.protocol_params.spend(
    init,
    Some(datum),
    FullUpgrade,
    input.output_reference,
    tx,
  )
}

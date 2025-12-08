use aiken/builtin
use aiken/collection/list.{concat}
use aiken/fuzz.{
  and_then, bool, bytearray_between, bytearray_fixed, constant, data,
  int_between, list_between, map, option,
}
use cardano/address.{Address, Script}
use cardano/assets.{Value, add, from_asset, from_lovelace, merge}
use cardano/fuzz.{address_with, stake_credential} as f
use cardano/script_context.{ScriptContext, Withdrawing}
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction, placeholder,
}
use multisig
use types.{NightProtocolParams}
use utils.{master_token_name}

const hash = #"f94a7d9092d5115d23b2d0d9179578faaccbe634903b8ba15e6478f5"

const keys =
  [
    #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
    #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
  ]

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

const multisig_transaction =
  Transaction {
    ..placeholder,
    reference_inputs: [input],
    extra_signatories: keys,
  }

const multisig_script_context =
  ScriptContext(multisig_transaction, 0, Withdrawing(Script(hash)))

fn create_input(base: Input) -> Fuzzer<Input> {
  let output <- and_then(create_output(base.output))

  let input = Input { output_reference: out_ref, output }
  modify_output_reference(input)
}

fn create_output(base: Output) -> Fuzzer<Output> {
  modify_all_output(base)
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

// ---------------------------------------------
// Multisig
// --------------------------------------------

test multisig_1() {
  multisig.multisig.else(hash, multisig_script_context)
}

test multisig_input_datum_keys(
  other_datum via modify_datum_keys(datum),
) fail once {
  let input =
    Input(out_ref, Output { ..output, datum: InlineDatum(other_datum) })

  let tx = Transaction { ..multisig_transaction, reference_inputs: [input] }
  multisig.multisig.else(
    hash,
    ScriptContext { ..multisig_script_context, transaction: tx },
  )
}

test multisig_input_param_datum_min_sigs(
  other_datum via modify_datum_min_sigs(datum),
) fail once {
  let input =
    Input(out_ref, Output { ..output, datum: InlineDatum(other_datum) })

  let tx = Transaction { ..multisig_transaction, reference_inputs: [input] }
  multisig.multisig.else(
    hash,
    ScriptContext { ..multisig_script_context, transaction: tx },
  )
}

test multisig_no_inputs(
  other_inputs via list_between(create_input(empty_input), 0, 10),
) fail {
  let tx =
    Transaction { ..multisig_transaction, reference_inputs: other_inputs }
  multisig.multisig.else(
    hash,
    ScriptContext { ..multisig_script_context, transaction: tx },
  )
}

test multisig_close_inputs(
  other_inputs via list_between(create_input(input), 1, 10),
) fail {
  let tx =
    Transaction { ..multisig_transaction, reference_inputs: other_inputs }
  multisig.multisig.else(
    hash,
    ScriptContext { ..multisig_script_context, transaction: tx },
  )
}

test multisig_other_inputs(
  other_inputs via list_between(create_input(input), 0, 10),
) {
  let tx =
    Transaction {
      ..multisig_transaction,
      reference_inputs: concat(other_inputs, [input]),
    }
  multisig.multisig.else(
    hash,
    ScriptContext { ..multisig_script_context, transaction: tx },
  )
}

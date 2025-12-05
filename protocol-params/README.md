# Midnight Protocol Params

## Midnight Multsig Token Validator Spec

### Withdraw Purpose

#### Inputs Validation

- _(1)_ There must be a reference input which we call the protocol params ref input.
  - _(1.1)_ The protocol params ref input value must only contain ADA
    and the previously minted Protocol Params NFT where the policy id is
    equal to the `protocol_params_policy` that parameterized the script.
    The presence of ADA is enforced by the ledger.
  - _(1.2)_ The protocol params ref input must contain an `InlineDatum`.
    - _(1.2.1)_ The datum must be of type `NightProtocolParams`.
    - _(1.2.2)_ The number of the `NightProtocolParams` field `tge_agent_auth_keys`
      that match an existing signatory from the `Transaction` field
      `extra_signatories` must be greater than or equal to the
      `NightProtocolParams` field `min_auth_signature`.

## Protocol Params Spec

This is a multivalidator, which means that the same script bytes/hash is used
to handle multiple purposes. The spend logic and mint logic is handled by the
same script.

### Minting Purpose

#### Inputs Validation

- _(1)_ Check that the parameterized output reference is found
  in one of the inputs.

#### Mint Validation

- _(2)_ There must be exactly one minted token (Protocol Params NFT).
  - _(2.1)_ The policy_id must match the scripts own policy id.
    The own policy id is passed in by the ledger.
  - _(2.2)_ The token name must be `"NightProtocolParams"`.
  - _(2.3)_ The amount must be `1`.

#### Output Validation:

- _(3)_ There must be an output which we call the protocol params output.
  - _(3.1)_ The protocol params output payment credential
    must match the script itself.
  - _(3.2)_ The protocol params output value must only contain ADA
    and the minted value from above. The presence of ADA is enforced
    by the ledger.
  - _(3.3)_ The protocol params output must contain an `InlineDatum`.
    - _(3.3.1)_ The inline datum must be of type `NightProtocolParams`.
    - _(3.3.2)_ The `NightProtocolParams` field `min_auth_signature`
      must be greater than `0`.
    - _(3.3.3)_ The `NightProtocolParams` field `tge_agent_auth_keys`
      must be a sorted and unique list of ByteArrays which are of 28 bytes
      in length. These are pubkey hashes which are 28 bytes in length.

### Spending Purpose - UpdateParameters

#### Input Validation

- _(1)_ There will be an input which we call the protocol params input.
  - _(1.1)_ The payment credential for the protocol params input is the
    executing script's hash. This is enforced by the ledger.
  - _(1.2)_ The protocol params input must contain a datum.
    - _(1.2.1)_ The datum must be of type `NightProtocolParams`.
    - _(1.2.2)_ The number of the `NightProtocolParams` field `tge_agent_auth_keys`
      that match an existing signatory from the `Transaction` field
      `extra_signatories` must be greater than or equal to the
      `NightProtocolParams` field `min_auth_signature`.

#### Output Validation

- _(2)_ There must be an output which we call the protocol params output.
  - _(2.1)_ The protocol params output address's payment credential must equal
    the protocol params input address's payment credential
  - _(2.2)_ The protocol params output value must only contain ADA
    and the previously minted Protocol Params NFT where the policy id is
    equal to the executing script's hash. The presence of ADA is enforced
    by the ledger.
  - _(2.3)_ The protocol params output must contain an `InlineDatum`.
    - _(2.3.1)_ The datum must be of type `NightProtocolParams`.
    - _(2.3.2)_ The `NightProtocolParams` field `min_auth_signature`
      must be greater than `0`.
    - _(2.3.3)_ The `NightProtocolParams` field `tge_agent_auth_keys`
      must be a sorted and unique list of ByteArrays which are of 28 bytes
      in length. These are pubkey hashes which are 28 bytes in length.

### Spending Purpose - FullUpgrade

#### Input Validation

- _(1)_ There will be an input which we call the protocol params input.
  - _(1.1)_ The protocol params input must contain a datum.
    - _(1.1.1)_ The datum must be of type `NightProtocolParams`.
    - _(1.1.2)_ The number of the `NightProtocolParams` field `tge_agent_auth_keys`
      that match an existing signatory from the `Transaction` field
      `extra_signatories` must be greater than or equal to the
      `NightProtocolParams` field `min_auth_signature`.

## Development

Validators (scripts) in the `validators` folder, and supporting functions in the `lib` folder using `.ak` as a file extension.

### Building

```sh
aiken build
```

### Configuring

**aiken.toml**

```toml
[config.default]
network_id = 41
```

Or, alternatively, write conditional environment modules under `env`.

## Testing

You can write tests in any module using the `test` keyword. For example:

```aiken
use config

test foo() {
  config.network_id + 1 == 42
}
```

To run all tests, simply do:

```sh
aiken check
```

To run only tests matching the string `foo`, do:

```sh
aiken check -m foo
```

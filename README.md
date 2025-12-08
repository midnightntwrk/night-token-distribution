# Midnight Glacier Drop on Cardano

This repository contains code and documents related to the distribution of Midnight's NIGHT tokens.  The distribution program is called the [Midnight Glacier Drop](https://www.midnight.gd/) program.

## About the Midnight Glacier Drop

The distribution program occurs in phases:
1. **Glacier Drop** - All the tokens are allocated to addresses on a set of widely used blockchains and can be claimed by eligible participants.
2. **Scavenger Mine** - Tokens unclaimed during the previous phase are processed as participants interact with a system that requires proof of having solved certain cryptographic problems.
3. **Lost-and-Found** - Participants who would have been eligible to claim in the Glacier Drop phase, but who did not, can still claim a fraction of their allocated NIGHT.

The first two phases occur prior to the launch of the Midnight blockchain, so they cannot occur on Midnight itself.  Instead, the Cardano blockchain provides the infrastructure.  The process of claiming NIGHT in these first two phases associates the claimed NIGHT with a Cardano address (the "destination" address).

When NIGHT is first claimed, it is "frozen."  (Thus the name Glacier Drop.)  During this time, it cannot be transferred.  After the completion of the first two phases, the NIGHT begins to "thaw"; it can be transferred to its destination wallet address and then held or transferred, as the owner chooses.  Transferring claimed NIGHT to its destination is called *redeeming* the claim or the *redemption* process.

## About This Code

The code in this repository implements the Cardano smart contracts that define NIGHT and govern the claim and redemption processes.  You will see the acronym `MGDOC` in directory and variable names; it stands for "Midnight Glacier Drop on Cardano."

The main subdirectories here are:
* `mgdoc/src` - contains the Plutus code that governs the smart contracts on Cardano and checks the validity of transactions
* `mgdoc/transactions` - contains the off-chain code that constructs the transactions
* `protocol-params` - contains Aiken code that implements a set of controlling parameters for the Night contract, protected by multi-signature scheme

## What Else Is Here?

This code has been audited by trusted third parties, and the audit reports are also here.
* `glacier-drop-audit` - contains the report on the audit of the `mgdoc` and `protocol-params` code

## License

This code is provided by the Midnight Foundation and licensed under the Apache License, version 2.0.

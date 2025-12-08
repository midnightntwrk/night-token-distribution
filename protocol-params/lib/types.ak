use cardano/address.{Address, Credential}

pub type FoundationWallet {
  address: Address,
  amount: Int,
}

pub type NightProtocolParams {
  // This will be the parameterized multisig validator once
  // the datum is put on chain
  dynamic_minting_logic: Credential,
  // The script hash of the hydra thread script. 
  hydra_thread_script_hash: ByteArray,
  // This will be set to the future reserve script via UpdateParameters
  reserve_script_address: Address,
  supply_script_address: Address,
  foundation_wallets: List<FoundationWallet>,
  tge_agent_auth_keys: List<ByteArray>,
  min_auth_signatures: Int,
}

pub type UpgradeParamsAction {
  UpdateParameters
  FullUpgrade
}

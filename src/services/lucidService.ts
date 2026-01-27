import { Lucid, Blockfrost, Data, UTxO, TxHash } from 'lucid-cardano';

// Escrow Datum type matching Plutus contract
const EscrowDatumSchema = Data.Object({
  buyer: Data.Bytes(),
  seller: Data.Bytes(),
  deadline: Data.Integer(),
});

type EscrowDatum = {
  buyer: string;
  seller: string;
  deadline: bigint;
};

// Escrow Redeemer - 0 = Release, 1 = Refund
type EscrowRedeemer = bigint;

export interface LucidConfig {
  network: 'Mainnet' | 'Preprod' | 'Preview';
  blockfrostApiKey?: string;
}

export interface EscrowParams {
  sellerAddress: string;
  amount: bigint; // in Lovelace
  deadline: Date;
}

class LucidService {
  private lucid: Lucid | null = null;
  private scriptAddress: string | null = null;
  private scriptCbor: string | null = null;

  async initialize(config: LucidConfig): Promise<void> {
    const networkUrl = config.network === 'Mainnet'
      ? 'https://cardano-mainnet.blockfrost.io/api/v0'
      : config.network === 'Preprod'
        ? 'https://cardano-preprod.blockfrost.io/api/v0'
        : 'https://cardano-preview.blockfrost.io/api/v0';

    // Use Blockfrost if API key provided, otherwise use a public provider
    if (config.blockfrostApiKey) {
      this.lucid = await Lucid.new(
        new Blockfrost(networkUrl, config.blockfrostApiKey),
        config.network
      );
    } else {
      // For demo/testnet without Blockfrost, use emulator or public endpoint
      this.lucid = await Lucid.new(undefined, config.network);
    }
  }

  async connectWallet(walletApi: any): Promise<string> {
    if (!this.lucid) throw new Error('Lucid not initialized');
    
    this.lucid.selectWallet(walletApi);
    return await this.lucid.wallet.address();
  }

  setScriptAddress(address: string, cbor: string): void {
    this.scriptAddress = address;
    this.scriptCbor = cbor;
  }

  async createEscrow(params: EscrowParams): Promise<TxHash> {
    if (!this.lucid) throw new Error('Lucid not initialized');
    if (!this.scriptAddress) throw new Error('Script address not set');

    const address = await this.lucid.wallet.address();
    const buyerPkh = this.lucid.utils.getAddressDetails(address).paymentCredential?.hash;
    const sellerPkh = this.lucid.utils.getAddressDetails(params.sellerAddress).paymentCredential?.hash;

    if (!buyerPkh || !sellerPkh) {
      throw new Error('Invalid address - cannot extract payment credential');
    }

    // Create datum matching Plutus contract
    const datum: EscrowDatum = {
      buyer: buyerPkh,
      seller: sellerPkh,
      deadline: BigInt(params.deadline.getTime()),
    };

    const tx = await this.lucid
      .newTx()
      .payToContract(
        this.scriptAddress,
        { inline: Data.to(datum as unknown as Data, EscrowDatumSchema as unknown as Data) },
        { lovelace: params.amount }
      )
      .complete();

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    return txHash;
  }

  async releaseEscrow(utxo: UTxO): Promise<TxHash> {
    if (!this.lucid) throw new Error('Lucid not initialized');
    if (!this.scriptCbor) throw new Error('Script CBOR not set');

    const address = await this.lucid.wallet.address();

    // Redeemer 0n = Release
    const redeemer: EscrowRedeemer = 0n;

    const tx = await this.lucid
      .newTx()
      .collectFrom([utxo], Data.to(redeemer))
      .attachSpendingValidator({ type: 'PlutusV2', script: this.scriptCbor })
      .addSigner(address)
      .complete();

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    return txHash;
  }

  async refundEscrow(utxo: UTxO): Promise<TxHash> {
    if (!this.lucid) throw new Error('Lucid not initialized');
    if (!this.scriptCbor) throw new Error('Script CBOR not set');

    const address = await this.lucid.wallet.address();

    // Redeemer 1n = Refund
    const redeemer: EscrowRedeemer = 1n;

    // Set validity interval to be after deadline
    const datum = Data.from(utxo.datum!) as unknown as EscrowDatum;
    const validFrom = Number(datum.deadline);

    const tx = await this.lucid
      .newTx()
      .collectFrom([utxo], Data.to(redeemer))
      .attachSpendingValidator({ type: 'PlutusV2', script: this.scriptCbor })
      .addSigner(address)
      .validFrom(validFrom)
      .complete();

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    return txHash;
  }

  async getScriptUtxos(): Promise<UTxO[]> {
    if (!this.lucid) throw new Error('Lucid not initialized');
    if (!this.scriptAddress) throw new Error('Script address not set');

    return await this.lucid.utxosAt(this.scriptAddress);
  }

  async getWalletBalance(): Promise<bigint> {
    if (!this.lucid) throw new Error('Lucid not initialized');
    
    const utxos = await this.lucid.wallet.getUtxos();
    return utxos.reduce((sum, utxo) => sum + (utxo.assets.lovelace || 0n), 0n);
  }

  isInitialized(): boolean {
    return this.lucid !== null;
  }

  hasScriptConfigured(): boolean {
    return this.scriptAddress !== null && this.scriptCbor !== null;
  }
}

export const lucidService = new LucidService();

// Helper to convert ADA to Lovelace
export const adaToLovelace = (ada: number): bigint => BigInt(Math.floor(ada * 1_000_000));

// Helper to convert Lovelace to ADA
export const lovelaceToAda = (lovelace: bigint): number => Number(lovelace) / 1_000_000;

// Generate a mock tx hash for simulation when no real blockchain
export const generateMockTxHash = (): string => {
  const chars = 'abcdef0123456789';
  let hash = '';
  for (let i = 0; i < 64; i++) {
    hash += chars[Math.floor(Math.random() * chars.length)];
  }
  return hash;
};

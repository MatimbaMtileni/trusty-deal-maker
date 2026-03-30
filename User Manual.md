# Trusty Deal Maker User Manual

**Author:** Matimba Mtileni  
**Latest Update Date:** March 30, 2026  

## 1. Introduction

**Trusty Deal Maker** is a decentralized escrow application built on the Cardano blockchain, utilizing React for its frontend and Supabase for its backend services. It facilitates secure peer-to-peer transactions by allowing buyers and sellers to create, fund, and settle escrows. The application also supports in-app messaging, file attachments, and email notifications for important events.

---

## 2. Key Features

### Escrow Lifecycle
Users can:
- Create new escrows  
- Fund escrows with Cardano (ADA)  
- Release funds to the seller upon successful completion  
- Request refunds if the deal falls through  

### In-App Messaging
- Real-time communication powered by Supabase Realtime  
- Enables direct interaction between buyers and sellers  

### File Attachments
- Attach documents or media securely  
- Files are stored privately  

### Email Notifications
- Receive updates for:
  - Escrow funding  
  - Fund release  
  - Refund initiation  

### Cardano Wallet Integration
- Connect CIP-30 compatible wallets:
  - Nami  
  - Lace  
  - Eternl  
  - Flint  
  - GeroWallet  
  - Typhon  
  - Yoroi  
  - Vespr  

### Enhanced Security
- Row-Level Security (RLS) policies protect:
  - Messages  
  - Attachments  
- Ensures only authorized users can access data  

---

## 3. Getting Started

To use Trusty Deal Maker, you need:
- A compatible Cardano wallet  
- An internet connection  

### 3.1 Connecting Your Wallet

1. **Install a Wallet**  
   Install a CIP-30 compatible wallet browser extension.

2. **Access the Application**  
   Open Trusty Deal Maker in your browser.

3. **Connect Wallet**  
   - Click the **"Connect Wallet"** button  
   - Select your wallet  
   - Approve the connection in your wallet  

4. **Wallet Linking (Important)**  
   - Your wallet address links to your profile automatically when signed in  
   - Required for secure messaging  

---

## 4. Escrow Lifecycle

### 4.1 Creating an Escrow

1. Initiate a new escrow  
2. Provide:
   - Amount  
   - Counterparty address  
   - Agreement description  
3. Review all details before proceeding  

---

### 4.2 Funding the Escrow (Buyer)

1. Deposit ADA into the escrow smart contract  
2. Confirm the transaction via your wallet  
3. Funds are securely held until the next action  

---

### 4.3 Releasing Funds (Buyer)

1. Confirm agreement completion  
2. Sign the transaction with your wallet  
3. Funds are transferred to the seller  

---

### 4.4 Refunding Funds (Buyer)

1. Trigger refund if:
   - Deadline passed  
   - Dispute occurred  
2. Sign refund transaction  
3. Funds return to buyer  

---

## 5. Messaging & Notifications

### 5.1 In-App Messaging

- Real-time chat between parties  
- Protected by RLS security  
- Supports file attachments  

---

### 5.2 Email Notifications

- Receive alerts for:
  - Funding events  
  - Fund releases  
  - Refunds  

**Note:** Ensure your email is correctly linked to your account.

---

## 6. Troubleshooting Messaging Failures

If messaging is not working:

- **Authentication**  
  Ensure you are logged in  

- **Wallet Linking**  
  Verify wallet is linked to your profile  

- **Browser Console**  
  Check for errors in developer tools  

- **Supabase Logs**  
  Review `send-notification` function logs for failures  

---

## 7. Glossary

- **ADA**  
  Native cryptocurrency of Cardano  

- **Cardano**  
  Public blockchain platform  

- **CIP-30**  
  Standard for wallet-to-dApp communication  

- **Escrow**  
  A system where funds are held by a third party until conditions are met  

- **Plutus**  
  Smart contract platform for Cardano (Haskell-based)  

- **RLS (Row-Level Security)**  
  Restricts database access at row level  

- **Smart Contract**  
  Self-executing code-based agreement  

- **Supabase**  
  Open-source backend platform (database, auth, realtime)  

---

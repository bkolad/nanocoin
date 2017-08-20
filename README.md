Nanocoin
=========

Nanocoin is a PoC cryptocurrency written in Haskell. It is under development to 
illustrate that the foundations of DLTs (Distributed Ledger Techonology) and aid
in dispelling the air of mystery about what a cryptocurrency is. The project attempts
to present the necessary cryptographic components and distributed systems knowledge
necessary to implement a cryptocurrency.

**Project goals**:

* Well-documented.
* Low-dependencies.
* Using classic proof-of-work.
* Simple P2P Protocol using UDP chatter.
* Supports chain reconfiguration.
* Basic ECDSA Block/Transaction signatures & validation
* Transfer & CreateAccount Transactions
* In-memory.

This document serves as a brief overview of blockchain components and how to
implement a cryptocurrency from scratch. Basic blockchain knowledge is assumed, but
brief overviews of the basic components are included.

Running a Node 
---------------

Install the [Stack](https://docs.haskellstack.org/en/stable/README/) build system:

```bash
$ stack setup
$ stack install nanocoin
$ nanocoin
```

Running `nanocoin` will spin up a Node with an RPC server running on `localhost:3000`
and a P2P server communicating with basic UDP Multicast on port `8001`.

You can specify which port th run the RPC server on, and from which directory to load
a node's public/private ECC key pair. If you do not supply a `KEYS_DIR`, the node will
generate a random key pair and issue a `CreateAccount` transaction to the network.

`Usage: nanocoin [-p|--rpc-port RPC_PORT] [-k|--keys KEYS_DIR]`

### RPC Interface

Nanocoin's RPC interface is implemented via an HTTP web server that serves as
both a command and query entry points. Simply type in
`localhost:XXXX/<cmd-or-query>` to interact with the node1:

#### Queries

`/address`:

    View the address of the current node (derived from the nodes public key)

`/blocks`:
   
    View the blocks on the block chain, including their transactions.

`/mempool`:
 
    View the current collected transactions that have not yet been included in a
    block on the network.

`/ledger`:
  
    View the current state of the ledger, representative of all the transactions
    of all the blocks applied in order to result in a cumulative ledger state.

#### Commands

`/mineBlock`:

    Attempt to mine a block containing the transactions currently in the node's mempool.
    This will fail if there are no transactions in the mempool or if the node
    has not joined the network by issuing a `CreateAccount` transaction.

`/createAccount`:

    Issues a `CreateAccount` transaction to the network. This endpoint must be
    visited for a node to join the network. Otherwise, other nodes in the
    network will not have this node's public key in memory such that
    transactions and mined block signatures can be verified.

`/transfer/:toAddress/:amount`
 
    Issues a `Transfer` transaction to the network, transferring the specified 
    `amount` of Nanocoin from this node's account to another node's account 
    designated by `toAddress`. If you try to transfer more Nanocoin than you
    have, the transaction will be rejected during the block mining process and
    purged from all nodes' mem-pools.

### TODO:

- [ ] Moar Docs (explain the crypto math a bit better).
- [ ] Transition from multicast to distributed processing p2p network
  (`cloud-haskell`)
- [ ] Include a *merkle tree root* of transaction hashes in the block header
  instead of a full list of transactions
- [ ] Develop a more sophisticated asynchronous messaging protocol after
  transitioning to `cloud-haskell`
- [ ] Add a CLI for friendlier interaction with a nanocoin node

Cryptography
------------

There are many cryptographic components that come together to make block chains and
cryptocurrency implementations more secure. Nanocoin uses Haskell's most notable 
cryptography library known as [`cryptonite`](https://hackage.haskell.org/package/cryptonite) 
as it provides all the cryptographic functions and datatypes relevant to a cryptocurrency
implementation.

### Hash Functions

Hash functions are pure, *one-way* functions for which every input a unique,
fixed-length output is produced, revealing no information about the input. *Strong* 
hash functions produce output that can be thought of as a digital fingerprint of the 
input data. The most important thing to note is that good hash functions obfuscate the original 
input but the most minor changes in the input data result in dramatically different outputs.

Example:
```haskell
ghci> import Data.ByteString
ghci> import Crypto.Hash (hashWith)
ghci> hashWith SHA3_256 ("1234" :: ByteString)
1d6442ddcfd9db1ff81df77cbefcd5afcc8c7ca952ab3101ede17a84b866d3f3
ghci> hashWith SHA3_256 ("12345" :: ByteString)
7d4e3eec80026719639ed4dba68916eb94c7a49a053e05c8f9578fe4e5a3d7ea
```
*Hashes are usually displayed in base16 format for brevity*

Since hash functions can be thought of a digital fingerprints of data, it is often an
effective and efficient way for two parties to confirm that they possess the same data.
Instead of both parties having to share their original copy of the data they wish to compare,
they can each share with each other the hash of the data and compare a much smaller piece of data
(assuming they use the same hash function).

**Collisions:**

Since hash algorithms produce a fixed sized output, i.e. there is a finite number of outputs and 
an infinite number of inputs, there is a chance that two inputs produce the same output. However, 
the chance of collisions for modern popular hashing algorithms are negligible, and when 
*collision attacks* become viable attack vectors, academia and industry are **usually** prepared.
the former with a more secure hashing algorithm and the latter with the adoption of the new algorithm.

#### SHA3_256 

Sha3_256 (Secure Hash Algorithm 3) is one of the more popular hashing algorithms, in which every 
input is converted into a 256 bit output. It's implementation is very complex, but yields a fast and
secure algorithm with which collisions will not feasibly happen given todays
computing power. 

### Finite Fields

A Finite Field `GF(p)` can be described as a cyclic group with a prime order, or `Z mod p` 
(the set of integers `Z` modulus a prime number *p*), closed over addition (`+`) and multiplication
(`*`) operations. The result of each operation is `mod p`. 

| `+` | 0 | 1 | 2 |
| --- | - | - | - |
| 0 | 0 | 1 | 2 |
| 1 | 1 | 2 | 0 | 
| 2 | 2 | 0 | 1 |

| `*` | 0 | 1 | 2 |
| --- | - | - | - |
| 0 | 0 | 0 | 0 |
| 1 | 0 | 1 | 2 | 
| 2 | 0 | 2 | 1 |

Visit [this link](https://eng.paxos.com/blockchain-101-foundational-math) for a more 
in depth dive into Finite Fields mathematics.

### ECC 

Elliptic curve cryptography (ECC) is an approach to public-key cryptography based on the algebraic 
structure of elliptic curves over finite fields with very large prime orders. This version of public key
cryptography provides the benefit of equal security with the public and private
keys needing fewer bits than tradition cryptographic schemes based on the discrete log problem. 

#### Elliptic Curves

In the `cryptonite` library, a `Curve` and `Point` are defined as follows:

```haskell
-- | Define either a binary curve or a prime curve.
data Curve = | CurveFP  CurvePrime  -- ^ 𝔽p

-- | Define an elliptic curve in 𝔽p.
-- The first parameter is the Prime Number.
data CurvePrime = CurvePrime Integer CurveCommon

-- | Define common parameters in a curve definition
-- of the form: y^2 = x^3 + ax + b.
data CurveCommon = CurveCommon
  { ecc_a :: Integer -- ^ curve parameter a
  , ecc_b :: Integer -- ^ curve parameter b
  , ecc_g :: Point   -- ^ base (generator) point
  , ecc_n :: Integer -- ^ order of G
  , ecc_h :: Integer -- ^ cofactor
  }

-- | Define a point on a curve.
data Point = Point Integer Integer
           | PointO -- ^ Point at Infinity
```

Visit [this link](https://eng.paxos.com/blockchain-101-elliptical-curve-cryptography)
for a more in depth dive into the math behind elliptic curves.

#### Public/Private Key Pairs

ECC defines public keys to be a point on the Elliptic Curve, and a private key
as a secret number *k* within the order of the curve (e.g. if the curve's order is
17, the private key will be a number *n* ∈ {0,.. ,17}). Public keys are derived from 
private keys by multiplying the generator point *G* by the private scalar *k*. 

This scheme yields a result similar to the discrete log problem, but instead of
it being difficult to recover a secret exponent *x* in the equation `g^x mod p`,
it is difficult to recover the secret scalar *k* in the equation `Gk mod p`.

In the `cryptonite` library, these keys are defined with the following data
structures: 

Publie Key:
```haskell
-- | ECDSA Public Key.
data PublicKey = PublicKey
  { public_curve :: Curve
  , public_q     :: PublicPoint
  } 
```

Private Key:
```haskell
type PrivateNumber = Integer

-- | ECDSA Private Key.
data PrivateKey = PrivateKey
  { private_curve :: Curve
  , private_d     :: PrivateNumber
  } 
```

#### Digital Signature Algorithm (ECDSA)

To sign a piece of data is to provide the two resulting integers from the ECDSA
(computed through a hash function). Inputs for the signing function are:

- The data as a string of bytes, *msg*.
- The elliptic curve private-key, *d*.

The **signing** algorithm will be described in terms of the curve Secp256k1, where
*p* is the secp256k1 prime, and *G* is it’s respective generator point:

1. Hash the document byte stream such that `z = H(msg)`
2. Generate a random value *k* ∈ {1,..,*p*−1}
3. Compute `(x,y) = kG`
4. Compute `r = x mod p`, if `r = 0` go back to step 1
5. Compute `s = (z + rd) / k` , if `s = 0` go back to step 1

The resulting (*r*,*s*) pair is the signature.

To **verify** the *signature* resulting from the signing algorithm the public
key corresponding to the private key used in the signing algorihthm is used.
Inputs to the verification algorithm are:

- The data as a string of bytes, *msg*
- The signature, (*r*,*s*)
- The EC public key *Q*

The output of the verification algorithm is simply a boolean indicating whether
the signature provide is indeed a valid signature of the given data correspoding 
to the private key with which it was signed. The algorithm is as follows:

1. Compute `z = H(msg)`
2. Compute `t = (z mod p) / s`
3. Compute `u = (r mod p) / s`
4. Let `(x,y) = tG + uQ`
5. Verify that `r = x mod p`

To sign and verify a piece of data using cryptonite:
```haskell
ghci> import Crypto.Number.Hash (SHA3_256)
ghci> import Crypto.PubKey.ECC.ECDSA (sign, verify)
ghci> import Crypto.PubKey.ECC.Generate (generate)
ghci> import Crypto.PubKey.ECC.Types (getCurveByName, SEC_p256k1)
ghci> let msg = "hello world" :: ByteString
ghci> let secp256k1 = getCurveByName SEC_p256k1
ghci> (pubKey, privKey) <- generate secp256k1
ghci> sig <- sign privKey SHA3_256 msg
ghci> verify SHA3_256 pubKey sig msg
True 
```

### Merkle Trees

TODO: Implementation and docs. Reference:
https://github.com/adjoint-io/merkle-tree

Distributed Ledgers (A.K.A. Blockchain)
---------------------------------------

Distributed Ledgers are *replicated state machines* that are kept in sync via 
P2P discovery and consensus protocols. The state that each node in the network 
(i.e. instance of this state machine) keeps track of is a **Ledger**, a data
structure that keeps track of transactions on the network. Each node's ledger is 
only modified when a block of transactions is broadcast to the network. Valid blocks
are determined by implementation specific *chain-rules* and consensus algorithms.

### Addresses

Addresses are succint representations of ECC Public Keys, and for all intents and purposes
can be thought of as a slightly more succinct digital fingerprint than a SHA3_256 hash.
The `deriveAddress` function maps a hash of a EC point to a unique, irreversible identity 
that uniquely defines a participant in the network and any participant can verify integrity
of it's coherence to a public key. 

For the Nanocoin project, we are simply copying Bitcoin's address implementation, plus a
few extra hashes for good measure. Addresses are usually base58 encoded for brevity, and to
distinguish them from SHA3_256 hashes which are encoded with base16.

```haskell
-- | address(x,y) = addrHash(string(x) <> string(y))
deriveAddress :: Key.PublicKey -> Address
deriveAddress pub = Address (b58 addr)
  where
    (x, y) = Key.extractPoint pub
    addr   = BA.convert $ deriveHash pstr
    pstr   = (i2osp x) <> ";" <> (i2osp y)

-- | addrHash(n) = sha256(sha256(ripemd160(sha256(n))))
deriveHash :: ByteString -> ByteString
deriveHash = Hash.sha256Raw'
           . Hash.sha256Raw'
           . Hash.ripemd160Raw
           . Hash.sha256Raw'
```

### Ledger

The `Ledger` is _the_ data structure that defines the replicated state to which modifications
can be made via **transactions**, and more specifically by accepting *blocks* of transactions
that are generated by miners, using a simple consensus algorithm. 

The Ledger maps addresses to accounts, where:
```haskell
-- | Datatype storing the holdings of addresses
newtype Ledger = Ledger
  { unLedger :: Map Address Account
  } deriving (Eq, Show, Generic, Monoid)
```

#### Accounts

Accounts in the Nanocoin DLT are simply ECC Public Keys. Accounts are created via
`CreateAccount` transactions, and are the data structure from which the **Ledger** 
is built. Accounts need to be introduced to the system via *transactions* because 
transactions are the means in which the ledger state is changed. By adding an account
(or Public Key) to the ledger, this allows nodes in the network to **verify** ECDSA 
signatures of both *transactions* submitted by the account, and *blocks* mined by
a node to which the account belongs. Before a node can submit valid transactions and/or
valid blocks to the network, it must issue a create account transaction.

```haskell 
type Account = (Key.PublicKey, Balance)
```

### Transactions

Transactions denote atomic modifications to the replicated state machines' state,
and are broadcast by nodes when user wish to issue a transaction on the network. 
Transactions are defined in Nanocoin as:

```haskell
data Transaction = Transaction
  { header    :: TransactionHeader
  , signature :: ByteString
  }
```

*Transaction Headers* must be signed with the private key corresponding to the `publicKey` field of 
`CreateAccount`, or the `issuer` field of Transfer transactions. The transaction's signature
is verified using the `ECDSA.verify` function in `cryptonite`.

**Transaction Headers**:

The `TransactionHeader` type denotes the type of transacation issued, and defines the type of
stateful modification that must be applied to the ledger, given the transaction is valid.

```haskell
data TransactionHeader
  = TxTransfer Transfer
  | TxAccount CreateAccount
```

#### CreateAccount

`CreateAccount` transactions are how node's join the network and become able to submit transactions
and/or mine blocks. Since transactions are signed with the private key relative to the ley in the 
`publicKey` field of the `CreateAccount` datatype, these transaction signatures are verified with respect
to the `publicKey` field (this is known as "self-signing" a transaction, and is only allowed in 
`CreateAccount` transactions. 

```haskell
data CreateAccount = CreateAccount
  { publicKey :: Key.PublicKey
  } 
```

`CreateAccount` transactions are issued via the `/createAccount` RPC endpoint, which pushes a
self-signed `CreateAccount` transaction into the network, and once this transaction has been included
in a block mined by another node, the node that issued the `CreateAccount` transaction will be able 
to submit `Transfer` transactions to the network and/or mine blocks. 

#### Transfer

`Transfer` transactions are the _main_ transaction issued to the network and is the principal 
ledger state modification transaction. This transaction transfers a given amount of Nanocoin
from one account to another. `Transfer` transaction headers are defined as:

```haskell
data Transfer = Transfer
  { issuer    :: Address
  , recipient :: Address
  , amount    :: Int
  }
```

This is the whole point of Nanocoin! To transact! The beauty is in the simplicity of this model.

#### Validation (Transaction)

For a node to validate a transaction:
1) The transaction `signature` must be verified against the transaction issuers private key using EDCSA `verify`.
2) The transaction header must be applied to the world state, and is valid if the ledger state modification is successful

The validation can be broken down into several parts:

```haskell
-- | Verifies the transaction signature by looking up the transaction origin by address,
-- and verifying the signature of the transaction header with the resulting public key.
verifyTransactionSig :: Transaction -> Ledger -> Either InvalidTx ()

-- | Applies a transaction to the ledger state by validating the transaction against the 
-- ledger state, and returning the modified the ledger state. 
applyTransaction :: Transaction -> Ledger -> Either InvalidTx Ledger

-- | If the account attempting to be added already exists, the transaction is invalid;
-- otherwise, the account is added to the ledger with an initial balance of 1000.
applyCreateAcount :: CreateAccount -> Ledger -> Either InvalidTx Ledger

-- | If both the sender and receiever accounts exist, and the sender has Nanocoin >= the
-- the amount being transferred, the transfer of funds is applied to the ledger state.
applyTransfer :: Transfer -> Ledger -> Either InvalidTx Ledger
```

Sometimes a transaction is only valid with respect to a ledger state resulting from a transaction that was
issued previous in the block, and therefore ledger state must be accumulated when applying transaction to the
ledger state.

### Blocks

Blocks are the representation of a list of transactions that have been validated, which represent an atomic
modification to the the ledger state. The word *Blockchain* comes from the fact that block headers reference previous
blocks that have been mined by nodes in the network. This chain starts from a **genesis block**, which represents an
initial ledger state from which each block in the chain builds upon. *Blockchains* are sequences of blocks 
representing lists of transactions that should be applied, atomically and sequentially to the ledger state, such 
that the final state after every block is replicated across all nodes in the network.

Blocks are represented in Nanocoin with the following data structures:

```haskell
type Index      = Int
type Timestamp  = Integer
type Blockchain = [Block]

data BlockHeader = BlockHeader
  { origin       :: Address       -- ^ Address of Block miner
  , previousHash :: ByteString    -- ^ Previous block hash
  , transactions :: [Transaction] -- ^ List of Transactions
  , nonce        :: Int           -- ^ Nonce for Proof-of-Work
  } deriving (Eq, Show, Generic, S.Serialize)

data Block = Block
  { index        :: Index         -- ^ Block height
  , header       :: BlockHeader   -- ^ Block header
  , signature    :: ByteString    -- ^ Block ECDSA signature
  }
```

Previous block hashes, contained in the `previousHash` field, are used to identify previous blocks in the chain
because of the way in which a change in even one bit of the data of the previous block will result in an incredibly different hash output. This creates a tamper resistant history due to the fact that before a node will accept a new 
block, it checks to see if hashing the previous block in it's local storage results in the same hash of the new 
block it has received. 

In Nanocoin, the blockchain is stored in the `nodeChain :: MVar [Block]` in which the most recent block is stored
as the `head` of the list. Chains with no blocks are inherently invalid, because a block chain must start with an 
initial *genesis block*.

The genesis block is defined in Nanocoin as:

```haskell
genesisBlock :: Block
genesisBlock = Block
  { index     = 0
  , header    = genesisBlockHeader
  , signature = ""
  }
  where
    genesisBlockHeader = BlockHeader
      { origin       = ""
      , previousHash = "0"
      , transactions = []
      , nonce        = 0
      }
```

#### Validation (Block)

Validation of a new block consists of the following:

1) The block `signature` must be verified against the block `origin` address's public key
2) The block `index` equal to the previous blocks index + 1
3) The computed hash of the local previous block must match the `previousHash` field in the new block.
4) The computed hash of the current block's header must satisfy the difficulty predicate in the PoW Algorithm
5) The block must have at least 1 transaction
6) Each transaction in the block header must be valid

The code that implements this is quite straight forward:
```haskell
data InvalidBlock
  = InvalidBlockSignature Text
  | InvalidBlockIndex
  | InvalidBlockHash
  | InvalidBlockNumTxs
  | InvalidBlockTx T.InvalidTx
  | InvalidPrevBlockHash
  | InvalidFirstBlock
  | InvalidOriginAddress Address
  | InvalidBlockTxs [T.InvalidTx]

-- | Validate a block before accepting a block as new block in chain
validateBlock
  :: Ledger
  -> Block
  -> Block
  -> Either InvalidBlock ()
validateBlock ledger prevBlock block
  | index block /= index prevBlock + 1                 = Left InvalidBlockIndex
  | hashBlock prevBlock /= previousHash (header block) = Left InvalidPrevBlockHash
  | not (checkProofOfWork block)                       = Left InvalidBlockHash
  | null (transactions $ header block)                 = Left InvalidBlockNumTxs
  | otherwise = do
  
      -- Verify signature of block
      verifyBlockSignature ledger block
      
      -- Validate all transactions w/ respect to world state
      first InvalidBlockTx $ do
        let txs = transactions $ header block
        T.validateTransactions ledger txs
```

Networking
-----------

Nanocoin is uses a simple P2P messaging protocol that uses UDP Multicast chatter. This means
that every message that is sent by a node on the network is broadcast simultaneously to every 
node on the network, i.e. every Nanocoin connected to the same router as the node sending the 
message. This greatly increases network traffic and is not suitable for a real world
implementation of a cryptocurrency. Future work on this project will transition from the simple
Multicast UDP chatter protocol to a distributed p2p network implemented using `cloud-haskell`. 

### NodeState

```
data NodeState = NodeState
  { nodeConfig   :: Peer                   -- ^ P2P info (rpc port, p2p port)
  , nodeChain    :: MVar Block.Blockchain  -- ^ In-memory Blockchain
  , nodeKeys     :: KeyPair                -- ^ Node key pair
  , nodeSender   :: MsgSender              -- ^ Function to broadcast a P2P message
  , nodeReceiver :: MsgReceiver            -- ^ The source of network messages
  , nodeLedger   :: MVar Ledger.Ledger     -- ^ In-memory ledger state
  , nodeMemPool  :: MVar MemPool.MemPool   -- ^ Mempool to collect transactions
  } 
```

At the core of each node is the node state. This datatype carries data relevant
to the operation of the node. The three `MVar`s are the in-memory representation
of the blockchain, the ledger, and the transaction pool. The rest of the values
are unchanging node configuration data.

#### MemPool

A *transaction mem-pool* is simply a data store for transaction broadcast to the
network. Each node keeps a list of the transaction that have been broadcast, and
updates this list each time a new `TransactionMsg` arrives from the
`MsgReceiver`; There are three times when a node's mempool is modified:

1) When a node receives a `TransactionMsg` with a valid transaction, it adds it
to the mempool
2) Whenever a new block is accepted and applied to the ledger state,
a node purges it's mempool of the transaction that were included in the new
block. 
3) When a new block is being mined, invalid transactions in the mempool are
removed from the mempool.

Nanocoin defines a Mempool as a newtype wrapper around a list.

```haskell
newtype MemPool = MemPool
  { unMemPool :: [Transaction]
  } deriving (Show, Eq, Generic, Monoid, ToJSON)
```

and supports 2 operations over the mempool:

```haskell
addTransaction :: Transaction -> MemPool -> MemPool
addTransaction tx (MemPool pool) = MemPool (pool ++ [tx])

removeTransactions :: [Transaction] -> MemPool -> MemPool
removeTransactions txs (MemPool pool) = MemPool $ pool \\ txs
```

The node's `MemPool` is stored in an `MVar` such that it persists the entire
duration of a node's execution, and can be potentially modified by multiple
processes safely.

### Messaging Protocol

The messaging protocol is defined as simple as possible, with the unfortunate result of 
greatly increasing the number of messages sent over the network:

```haskell
data Msg 
  = QueryBlockMsg Int
  | BlockMsg Block
  | TransactionMsg Transaction
```

Each node in the network is running a process that is listening on the multicast port 8001
and waits to receive messages, dispatching them to a function `handleMessage` that has the
following implementation:

```
handleMessage :: NodeState -> Msg -> IO ()
handleMessage nodeState msg = do
  case msg of
    ...
```

1. `QueryBlockMsg index`
   1. If `NodeState - nodeChain` has a block with the given index, broadcast the block to the network
   2. Else log the error and do nothing

2. `BlockMsg block`
   1. If genesis block is instantiated, attempt to apply the block to the ledger state.
   2. Else log the error and do nothing

3. `TransactionMsg transaction`
   1. If the transaction signature is valid, add it to the node's mempool
   2. Else log the error and do nothing

### Consensus Algorithm

Nanocoin uses a simple proof of work (PoW) approach to chain concensus: 

To mine a block on the chain, a node must compute a nonce such that that 
resulting hash of the block being mined begins with a number of 0's equal 
to `round(ln(n))` where `n` is the length of the current chain (i.e. the index
of the current block being mined); This predicate is known as the *block difficulty*. 
For this PoW implementation the average nonce computed is `16^n`, so when the length
of the chain surpasses 12 (`round(ln(n)) == 4`) it begins to take several seconds
to mine each block. As `n` surpasses 23, mining a block could take well over 10 minutes. 

**Note**: A *nonce* is an arbitrary positive number repeatedly incremented and added to the
block header to produce a hash that has the correct number of leading zeros, denoted by 
block difficulty.

```haskell
proofOfWork
  :: Int         -- ^ Difficulty measured by block index
  -> BlockHeader -- ^ Header to hash with nonce parameter
  -> BlockHeader
proofOfWork idx blockHeader = blockHeader { nonce = calcNonce 0 }
  where
    difficulty = calcDifficulty idx
    prefix = toS $ replicate difficulty '0'

    calcNonce n
      | prefix' == prefix = n
      | otherwise = calcNonce $ n + 1
      where
        headerHash = hashBlockHeader (blockHeader { nonce = n })
        prefix' = BS.take difficulty headerHash
        
calcDifficulty :: Int -> Int
calcDifficulty = round . logBase (2 :: Float) . fromIntegral
```

#### Validation (Consensus)

Blocks are validated with respect to Nanocoin's PoW consensus algorithm in a very simple way:

```haskell
checkProofOfWork :: Block -> Bool
checkProofOfWork block =
    BS.isPrefixOf prefix $ hashBlock block
  where
    difficulty = calcDifficulty $ index block
    prefix = toS $ replicate difficulty '0'
```

This is perhaps the one of the most notable outcomes of simple Proof of Work; Generating a new 
valid block is computationally difficult, but validating the generated block is computationally trivial.

Enabling Multicast
------------------

Nanocoin uses a UDP multicast gossip protocol for P2P networking. Before
running the program, make sure your localhost network interface has multicast
enabled. The simplest way to do this on Unix-like systems is:

```bash
$ sudo ifconfig lo multicast
```

Otherwise, the program with fail with `addMembership: failed (Unknown error -1)`
error from the `Multicast` module when attempting to add the node to the
multicast group. 

Usually, `lo` is the name of the local loopback (localhost) network interface on
modern Linux machines, but could differ depending on your machine and/or OS. To
check what your localhost network interface is, type `ifconfig`; The interface
prefixed by `lo`, e.g. `lo0` is usually the interface of interest. Simply
replace `lo` with the name of your localhost network interface in the command
above.

License
-------

Copyright 2017 Adjoint Inc

Released under Apache 2.0.

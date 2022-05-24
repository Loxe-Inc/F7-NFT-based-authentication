# NFT Based Authentication
A Fund 7 project to create an open source repository for NFT based authentication

This repo includes
- documentation in this README
- smart contracts code with relevant endpoints
- emulator trace based tests that demo the functionality

Plutus version: `v2022-04-06`
Commit hash: `e4062bca213f233cdf9822833b07aa69dff6d22a`

### The idea

This project showcases the potential use of NFT, for implementing OAuth 2.0 type authentication. The OAuth 2.0 authorization framework enables a third-party application to obtain limited access to an HTTP service, either on behalf of a resource owner by orchestrating an approval interaction between the resource owner and the HTTP service, or by allowing the third-party application to obtain access on its own behalf. For example, you would have used your social media accounts like Google, Facebook or Twitter to sign up and login to other websites. Behind scenes OAuth 2.0 enables the social login. Further details about OAuth 2.0 be found [here](https://oauth.net/2/) Potentially NFTs can be used for achieving the functionality of the OAuth protocol used for authentication in web applications. For example, the way you log in to a website using your Google or Facebook account.

### Implementation
In this scheme, there is an issuer smart contract called `AuthNFTIssuer` that mints and issues access tokens to authorised clients. The access tokens are represented by NFTs minted and delivered to the authorised wallet. There is another smart contract called `ProtectedResource` that acts as the custodian of the restricted resource. The protected smart contract grants access to a client wallet if it holds the access token. If it does not, then access is denied.

What makes the scheme secure is the way the access token NFT in minted. The token's currency symbol is built using the pkh of the Issuer whereas the token name is the pkh of the client wallet. The minting policy is parametrised with the issuer's pkh. This makes the NFT policy unique to the issuer as well as the client wallet. The protected smart contract checks a wallet using the pkh of that wallet for the present of the access token. So a given wallet should have the NFT issued by the issuer and also the token should match its own pkh to be granted access by the protected smart contract.

**Illustration**:
Refer to the diagram below. The client wallet is the one that requests for an authentication NFT. The Auth SC is the smart contract that mints the auth NFT and delivers to the requested client. Whether to authorise a client is decided by issuer and in practice can depend on various aspects.  If authorised, the issuer delivers the NFT to the client. The client can then request access ro the protected resource. The smart contract at the protected resource checks for the correct NFT and accordingly grants or denies access.
<img width="544" alt="nbauth" src="https://user-images.githubusercontent.com/5955141/169681829-7f2736c5-bd19-4b55-8626-cb785a2385c3.png">

This project gives uses the [Plutus Platform starter project](https://github.com/input-output-hk/plutus-starter) as the template.

### Setting up
Please refer to [Setting up](https://github.com/input-output-hk/plutus-starter#setting-up) section of the plutus starter project.

### The Plutus Application Backend (PAB)

With the PAB we can serve and interact with contracts over a web API. You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).

Here, the PAB is configured with two contracts, `AuthNFTIssuer` and  `ProtectedResource` contracts under `/src` folder.

1. Build the PAB executable:

```
cabal build nft-based-authentication
```

2. Run the emulator trace:

```
cabal repl
import Trace
> testContract
````

#### Endpoints
`AuthNFTIssuer.hs` has the endpoint:
- `getAccessToken` : It mints the access token NFT for the requestor wallet identified by a pkhs as input and sends it to that wallet.

`ProtectedResource.hs` has the following endpoints:
- `checkAccess` : This checks for the correct access token (auth NFT) at the given pubKeyHashAddress. If the address holds the NFT, access can be granted.

#### Future work
The scope of the proposal consisted of demonstration of OAuth like authentication with Plutus smart contracts wherein an access token issued by an issuer can be used by a checker to authenticate a holder wallet. As an extension to this work, the authentication scheme can include expiration time, authorities etc.

The concept of authentication using NFTs has become common these days wherein it is used for identifying holder wallets for airdrops and payments. In addition, we feel its possible to extend it to more complex schemes like OAuth. With this work as a precursor, we hope we see further progress catering to real world use cases of OAuth.

#### Support/Issues/Community
If you're looking for support using this repo or extending this work, please report it as a bug, suggestion or a feature request.


Thanks!

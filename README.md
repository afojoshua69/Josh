# Josh
Overview

This project implements a next-generation procurement system using the Clarity smart contract language on the Stacks blockchain.
It reimagines how Requests for Proposals (RFPs) can be created, submitted, evaluated, and awarded — with full transparency, immutability, and fairness, aligned with the vision of Google Clarity Web3.

The contract features sealed bid commit–reveal, reputation-weighted evaluations, milestone verification, and on-chain award badges for provable provenance.

✨ Key Features

🔒 Sealed Commit–Reveal Bidding – Vendors commit with hashed proposals, later reveal for fairness.

👩‍⚖️ Evaluator Governance – Only approved evaluators can score; votes weighted by reputation.

⚖️ Tie-Break Logic – Weighted average → vendor reputation → earliest reveal.

🏗 Milestone & Final Verification – Deliverables hashed on-chain; payments linked to approvals.

🏅 Award Badge NFT – Winners receive immutable on-chain recognition of contract awards.

💸 Escrow-Ready Design – Hook points for SIP-010 fungible token transfers included.

🔄 Workflow

RFP Creation – Client defines title, brief, deposit, and deadlines.

Commit Phase – Vendors commit their proposal hash.

Reveal Phase – Vendors reveal details, ensuring secrecy until deadline.

Evaluation Phase – Evaluators score vendors; scores weighted by reputation.

Finalization – Smart contract auto-selects the winner, mints award badge.

Execution – Milestones and final deliverables are posted, verified, and paid.

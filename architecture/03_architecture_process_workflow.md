## Process workflow

How does a piece of code become a plot, or a table of numbers browsable on the tafXplorer or downloadable through a url address?

```mermaid
flowchart TD

    Commit["GitHub Commit: <code>plot(1:10,1:10)</code>"]
    Sync[Trigger Import:</br>TAF Portal]
    Copy[Copy Code To Storage]
    Record[Record in Database]
    Check[Run Validation on Code]
    Decision{Validation<br/>Passed?}
    Fail[Store Failure Status]
    Run[Execute Assessment:</br>Create plot]
    Outputs["Store Outputs<br/>Plots Tables Reports"]
    Archive[Archive Results]
    Publish["Expose Results through APIs"]
    Xplorer["Expose Results through Explorer"]

    Commit e1@==> Sync
    Sync e2@==> Copy
    Copy --> Record
    Copy e3@==> Check

    Check e4@==> Decision

    Decision -- No --> Fail --> Record
    Outputs e10@--> Record

    Decision e5@== Yes ==> Run
    Run e6@==> Outputs
    Outputs e7@==> Archive
    Archive e8@==> Publish
    Publish e9@==> Xplorer

    Record e11@--> Publish

    e1@{ animate: true }
    e2@{ animate: true }
    e3@{ animate: true }
    e4@{ animate: true }
    e5@{ animate: true }
    e6@{ animate: true }
    e7@{ animate: true }
    e8@{ animate: true }
    e9@{ animate: true }

    e10@{ animate: true }
    e11@{ animate: true }

    click D href "https://www.github.com/ices-taf" "Open this in a new tab" _blank

    Xplorer --> D
  ```

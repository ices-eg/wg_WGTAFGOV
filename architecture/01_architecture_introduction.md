## TAF architecture: Introduction

#### Team composition

The current TAF team is composed of a thematic lead (full time), developer (2 days), student developer (2 days), graphical data scientist (2 days), strategy coordinator (3 days), process (0.5 days), with additional support from the data managent team lead, and developer team lead.

## Core vision for TAF

The main goal for TAF in 2026/2027 is to have a process that achieves the following:

```mermaid
flowchart LR

    GitHub@{ shape: tag-doc, label: <code>Assessment Code</code>}
    Validate@{ shape: flag, label: Automated Validation}
    Execute@{ shape: procs, label: "Automated Execution:</br>Diagnostics</br>QC"}
    Catalogue@{ shape: lin-cyl, label: Central Storage}

    API@{ shape: tri, label: Access}
    Explorer@{ shape: win-pane, label: "tafXplorer:</br>standard outputs" }
    Advice@{ shape: doc, label: "Quality Assured</br>Assessment Products"}

    GitHub --> Validate
    Validate --> Execute
    Execute --> Catalogue
    Catalogue --> API
    API --> Explorer
    Explorer --> Advice
  ```

That is:

- Code is submitted
- Automated validation
- Automated execution
  - standard plots and tables of input data
  - model diagnostics
  - standard plots and tables of outputs and results
- central storage
  - code
  - results
  - metadata
- data access
- tafXplorer: standardised online version of EG report
- public acess to detailed assessement products

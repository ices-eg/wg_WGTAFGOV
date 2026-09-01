# TAF Architecture

This document contains details on the design of TAF going forward from September 2026.

#### Team composition

The current TAF team is composed of a thematic lead (full time), developer (2 days), student developer (2 days), graphical data scientist (2 days), strategy coordinator (3 days), process (0.5 days), with additional support from the data managent team lead, and developer team lead.

### Document overview

1. High-level business architecture (easy for governance groups)
2. Technical component architecture (for developers)
3. Process flow
4. TAF Analysis Explorer architecture (future vision)
5. Strategic overview

## 1. High level

The stock assessment scientist is provided access to a github repository where they can work and develop thier stock assessment in the TAF format, refered to as `TAF Code`.  See [[TAF format]] for more details.  The TAF code is copied onto the TAF server

```mermaid
flowchart TB

    U["Stock Assessment<br/>Scientist"]
    Stakeholder["Advice Requesters<br/>Stakeholders<br/>Reviewers<br/>Public"]

    GH["GitHub Repository:<br/>TAF Code"]
    WEB["TAF Portal:<br/>view and manage runs"]
    ICESTAF["icesTAF R package:<br/>view and manage runs"]

    IS("Import service:<br/>TAF code and initial data")

    FS@{ shape: lin-cyl, label: "Repository Storage:<br/>TAF Server File System"}
    VAL["Validation Service:<br/>TAF QC Checks"]
    RUN("Execution Service:<br/>Run Assessment")
    DB[("TAF Database:<br/>metadata<br/>status")]
    API[ASP.NET Core API]
    EXPLORER["<b>TafXplorer</b>:<br/>'adviceXplorer' for expert group reports"]
    RESULTS["Standard Assessment Outputs:<br/>Plots<br/>Tables<br/>PDF Reports"]

    U -->|git| GH
    U -->|browser| WEB
    U -->|R| ICESTAF

    Stakeholder -->|R - readonly| ICESTAF

    WEB --> API
    ICESTAF --> API

    GH <--> VAL

    GH e1@--> IS
    e1@{ animate: true }
    API -.-> IS
    IS e2@--> FS
    e2@{ animate: true }

    FS <--> VAL
    FS <--> RUN
    FS --> DB

    API --> DB

    DB --> EXPLORER
    EXPLORER --> RESULTS

    Stakeholder -->|browser| EXPLORER
```

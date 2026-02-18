# oiReachtas

An R package for interacting with the [Houses of the Oireachtas Open Data API](https://api.oireachtas.ie/).

## Overview

`oiReachtas` provides a clean, tidy interface to Ireland's parliamentary open data. All functions return [tibbles](https://tibble.tidyverse.org/) ready for downstream analysis — sentiment analysis of debates, member activity tracking, voting patterns, legislative research, and more.

The API is fully public and requires no authentication.

## Installation

```r
# Install from GitHub
# install.packages("pak")
pak::pak("CathalByrneGit/oiReachtas")
```

## Available Functions

| Function | Description |
|---|---|
| `get_members()` | Members (TDs and Senators) |
| `get_debates()` | Parliamentary debate metadata |
| `get_debate_record()` | Full record for a single debate |
| `get_questions()` | Parliamentary questions (oral and written) |
| `get_divisions()` | Divisions (votes) |
| `get_division_votes()` | Individual member votes for a division |
| `get_legislation()` | Bills and Acts |
| `get_constituencies()` | Electoral constituencies |
| `get_parties()` | Political parties |
| `get_houses()` | Houses of the Oireachtas |

## Quick Start

```r
library(oiReachtas)

# Members of the current Dáil
dail_members <- get_members(chamber = "dail", house_no = 33)

# Debates in a date range
debates <- get_debates(
  chamber    = "dail",
  date_start = "2023-01-01",
  date_end   = "2023-03-31"
)

# Written parliamentary questions from a member
questions <- get_questions(
  question_type = "written",
  date_start    = "2023-01-01",
  date_end      = "2023-06-30"
)

# Dáil division (vote) results
votes <- get_divisions(
  chamber    = "dail",
  date_start = "2023-01-01",
  date_end   = "2023-12-31",
  all_pages  = TRUE
)

# How each TD voted in the first division
individual_votes <- get_division_votes(votes$division_id[1])

# Bills currently before the Dáil
current_bills <- get_legislation(bill_status = "Current", chamber = "dail")
```

## Pagination

By default, functions return up to 50 records. Use `limit` and `skip` for manual pagination, or `all_pages = TRUE` to retrieve everything automatically:

```r
all_members <- get_members(all_pages = TRUE)
```

## Debate Transcripts

Debate metadata includes a `formats_xml` URI pointing to the full transcript (XML in [Akoma Ntoso](http://www.akomantoso.org/) format) hosted at `https://data.oireachtas.ie/`:

```r
debates <- get_debates(chamber = "dail", date_start = "2023-01-17", date_end = "2023-01-17")
# debates$formats_xml[1] → URI for the XML transcript
```

## Analysis Ideas

- **Sentiment analysis**: Download debate transcripts and score member speech over time.
- **Topic modelling**: Identify each member's main concerns using LDA or BERTopic.
- **Voting cohesion**: Cluster members by division vote similarity.
- **Responsiveness**: Compare question topics before/after major news events.
- **Legislative tracking**: Follow bills from introduction to enactment.

## Data License

Data are provided under the [Oireachtas (Open Data) PSI Licence](https://www.oireachtas.ie/en/open-data/).

## API Reference

- API root: <https://api.oireachtas.ie/v1>
- Data files: <https://data.oireachtas.ie>
- Contact: open.data@oireachtas.ie

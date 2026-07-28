# addcols_does not regress on existing columns for a representative run

    {
      "type": "character",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["timestamp", "time", "distance", "lat", "lng", "altitude", "speed_ms", "speed_kmh", "pace", "heartrate", "lat_offset", "lng_offset"]
        }
      },
      "value": ["double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double"]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["column", "type", "n_na", "mean", "hash"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["timestamp", "time", "distance", "lat", "lng", "altitude", "speed_ms", "speed_kmh", "pace", "heartrate", "lat_offset", "lng_offset"]
        },
        {
          "type": "character",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["timestamp", "time", "distance", "lat", "lng", "altitude", "speed_ms", "speed_kmh", "pace", "heartrate", "lat_offset", "lng_offset"]
            }
          },
          "value": ["double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1362841851.72678566, 1589.72678571, 5070.73468006, 28.1345926, -80.60339743, 4.18, 3.23338894, 11.64020017, 5.26721086, 172.6875, -0.00134964, -0.02130426]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, null, null, null, null, null, null, null, null, null, null]
        }
      ]
    }


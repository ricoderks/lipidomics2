# lipidomics2 (development version)

* Support the alignment export format of MS-DIAL >= 5.5.261105. The MS-DIAL
  export format/version is detected automatically when uploading a file. For
  MS-DIAL >= 5.5.261105 annotations are kept when the column *Evidence source*
  is `RuleBased`. For older versions the *low score:* and
  *no MS2:* prefixes in the metabolite name are still used. Uploading positive
  and negative mode files from different MS-DIAL versions gives an error.
* Initial CRAN submission.

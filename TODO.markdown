# TODO

These are what I plan to work on as I find the time:

  * bucketedFold function to accumulate rows while a condition holds through,
  	performs bucket-level computation when the condition changes, outputs the
  	resulting set, flushes the accumulator, puts the latest row (the one with
  	the changed condition value) into the accumulator and moves on.

  * a way to implement multiple back-ends: CSV file, database (HDBC, Takusen,
  	etc), MongoDB, CouchDB, etc.

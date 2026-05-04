# Initialize Logging

This function initializes logging for the application.

## Usage

``` r
initLogging(log_level, log_file = NULL)
```

## Arguments

- log_level:

  A single character string specifying the log level. This should be one
  of 'TRACE', 'DEBUG', 'INFO', 'WARN', 'ERROR', or 'FATAL'.

- log_file:

  An optional character string specifying the name of the file where the
  log will be written. If this is NULL, the log will be written to the
  console.

## Value

None. This function is used for its side effects of initializing the
logging.

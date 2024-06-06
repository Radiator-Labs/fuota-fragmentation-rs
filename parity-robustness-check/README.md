# Parity-robustness-check

The parity-robustness-check is used to evaluate a given parity algorithm's effectiveness at recovering from missed packets. This is designed to randomly skip packets as would happen in a lossy transmission environment, and collect data across a large number of executions. The execution

## Usage

```bash
Usage: parity-robustness-check [OPTIONS]

Options:
  -d, --detailed               Output the detailed JSON output
  -c, --count <COUNT>          Number of test cycles to execute [default: 1]
  -s, --size <SIZE>            Size of the fragments [default: 40]
  -p, --parity <PARITY>        Amount of parity fragments as a fraction of the number of data fragments [default: 0.45]
  -s, --skip-rate <SKIP_RATE>  Percent of fragments to not skip [default: 0.02]
  -h, --help                   Print help
  -V, --version                Print version
```

## Improvements

### Near-term improvements

* Perform an equality check
  * Currently trusts the CRC check
* Support "bursty" packet loss
* Support inputting random number seeds to support issue replication
* Add better CI scripting
* Eliminate the CRC and Signature from the data
  * This is specific to Kelvin's application
  * Instead, pass in a validation check function that can evaluate the completed transfer in whatever manner a given project needs.

### Longer-term improvements

* Support inputting a binary file at the command line.
* Support generating binary files of random or fixed pattern data, of specified size.
* Shift randomization and replication into an existing tool, like quick-check
  * Not sure whether this offers any real improvement over the ad hoc implementation included.

## end

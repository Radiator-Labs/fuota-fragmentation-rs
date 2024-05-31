# Binary Fragmenter

The Binary Fragmenter is an application that converts a stripped binary (.bin) file into a JSON encoded file ready for operation with the FUOTA downloader.

## Usage

* bin-fragmenter <--file \<FILE>|--size \<SIZE>|--parity-pct \<PARITY_PCT>>

### Options

| Short | Long | Meaning |
|--|--|--|
| -f | --file \<FILE> | Name of the binary file, usually a stripped .bin file |
| -s | --size \<SIZE> | Length of a message in bytes [default: 50] |
| -p | --parity-pct <PARITY_PCT> | Parity percentage [default: 0.45] |
| -h | --help        | Print help |
| -V | --version     | Print version |

### Output

```bash
Loaded '<filename.bin>'. Size: 202312
Sz 50 #f 4048 #p 4452 crc 0x6a1220cf
```

### Limitations

The total number of packets (f + p) must be less than 2^14.

## end

# btinfo-json

This is simple tool to output the metadata from a BitTorrent file as JSON.

(I find it especially useful in combination with [`jq`](https://github.com/stedolan/jq).)

# Example Usage

```.sh
btinfo-json debian-9.5.0-arm64-netinst.iso.torrent
```

```json
{"announce":"http://bttracker.debian.org:6969/announce","comment":"\"Debian CD from cdimage.debian.org\"","creation date":1531571681,"hash":"c7794427addb6d18b2b5913d83a5fae6fb026e0c","httpseeds":["https://cdimage.debian.org/cdimage/release/9.5.0//srv/cdbuilder.debian.org/dst/deb-cd/weekly-builds/arm64/iso-cd/debian-9.5.0-arm64-netinst.iso","https://cdimage.debian.org/cdimage/archive/9.5.0//srv/cdbuilder.debian.org/dst/deb-cd/weekly-builds/arm64/iso-cd/debian-9.5.0-arm64-netinst.iso"],"info":{"length":211812352,"name":"debian-9.5.0-arm64-netinst.iso","piece length":262144,"pieces":null}}
```

# Current Limitations

- Poor error reporting.
- Any bencoded string that isn't UTF-8 encoded text is represented as `null` in the output.

# Building and Installation

I recommend using [stack](https://docs.haskellstack.org/en/stable/README/):

```sh
git clone https://github.com/thedward/btinfo-json.git
cd btinfo-json
stack build
stack install
```

By default stack will install the executable into `~/.local/bin`. So make sure that is in your `$PATH`.

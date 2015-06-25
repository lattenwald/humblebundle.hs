# HumbleBundle downloader #

I created this tool for downloading HumbleBundle games. Their native Android application is not that fast, and I decided I'd better download all APKs and store them in private cloud for easy access. This can also be used to download binaries for other platforms, though some functionality might be missing as I didn't test it for other binaries.

### Installation ###

* checkout repository
* cabal sandbox init
* cabal install --only-dependencies
* cabal build

Now you have binary in *dist/build/HumbleBundle-hs/HumbleBundle-hs*

### Using ###

First create file *credentials*, with two lines: login and password.

Then create sqlite3 database for storing md5 hashes

    sqlite3 hashes.db < "CREATE TABLE hashes (path TEXT, md5 TEXT, PRIMARY KEY (path));"

Finally, run something like

    ./dist/build/HumbleBundle-hs/HumbleBundle-hs -v -p Android -d ../cloud/HB +RTS -N8

### Options ###

    Usage: HumbleBundle-hs [-v|--verbose] [-p|--platform ARG] (-d|--destination ARG)
                           [-h|--hashes ARG]
      Download binaries from HumbleBundle
    
    Available options:
      -h,--help                Show this help text
      -v,--verbose             be verbose
      -p,--platform ARG        platform to download binaries for
      -d,--destination ARG     where to download binaries
      -h,--hashes ARG          file with hashes
# HumbleBundle downloader #

I created this tool for downloading HumbleBundle games. Their native Android application is not that fast, and I decided I'd better download all APKs and store them in private cloud for easy access. This can also be used to download binaries for other platforms, though some functionality might be missing as I didn't test it for other binaries.

### Things that do not work yet

Two-Step Verification is not supported yet.

### Installation ###

* checkout repository
* `stack build --copy-bins`

Now you have binary `HumbleBundle-hs` somewhere in our `$PATH` (in my case it is `$HOME/.local/bin/HumbleBundle-hs`)

### Using ###

For some information on command line options you can runeffect

    HumbleBundle-hs --help

To actually use it run something like

    HumbleBundle-hs -v -p Android -d ../cloud/HB +RTS -N8

Follow prompts. It will ask you for credentials and possible for humblehuard code if the site will need it. After successful authentication cookies are stored for subsequent runs in file `cookies`.

File hashes database is located at `hashes.bin` which is created after first successful run. If you run into problems, easiest solution is to remove it (and maybe `cookies` too) and re-run this tool.

### Options ###

    Usage: HumbleBundle-hs [-v|--verbose] [-p|--platform ARG] (-d|--destination ARG)
                           [-h|--hashes ARG]
      Download binaries from HumbleBundle

    Available options:
      -h,--help                Show this help text
      -v,--verbose             be verbose
      -p,--platform ARG        platform to download binaries for
      -d,--destination ARG     where to download binaries
      -h,--hashes ARG          file with hashes (without suffix)

# overlord-cli

Command line interface for [Overlord][].

Overlord is designed to be used from the Lisp REPL, so the command line interface is written on a client-server model.

## Running the server

To start the server, from Lisp, evaluate:

    (asdf:load-system "overlord-cli")
    (overlord-cli:start-server)
    
This will start the server in a background thread. To stop the server, evaluate:

    (overlord-cli:stop-server)
    
You can also stop the server from the client:

    $ overlord stop

You can start the server however you like – in its own process, in a REPL you have running, or so forth – but beware: currently Overlord should only be used in one Lisp process at a time (concurrent Lisp processes in different implementation are OK, however).
    
## Building the client executable

To build the client, on an implementation that supports saving an image, evaluate:

    (asdf:load-system "overlord-cli/client")
    (overlord-cli/client:save-client "~/overlord")

This will save an executable to `~/overlord` (or `~/overlord.exe` on Windows).

On ECL, use `asdf:make` instead:

    (asdf:make-system "overlord-cli/client")
    
This will create an executable in the same directory with `overlord-cli.asd`.

The client is as simple as possible, with minimal dependencies, so it builds fast and produces a relatively compact executable.

Once the client is built and in your `PATH`, for more guidance, run:

    $ overlord --help

This will list the available subcommands. More information on a subcommand can be obtained with a `help` parameter:

    $ overlord make --help

### Other ways to build the client

If you use Roswell, you can build the client the Roswell way using:

    ros install overlord-cli
    
If you use cl-launch, a trivial [script](cl-launch/overlord) is included.

## Basic usage

    $ overlord build <file> # Build a file
    $ overlord make  # Build the current system
    
## The current system

Some subcommands operate on the “current system”.

    $ overlord make # Build the current system.
    
All “the current system” means is that we find the nearest `.asd` file, parse out its name, and build the system by that name. E.g. if there is a `my-system.asd` file in the current directory, the “current system” is just `"my-system"`. (Note that this works even if the nearest ASDF file is not currently in the ASDF registry.)

## License

MIT

[Usocket]: https://common-lisp.net/project/usocket/
[Overlord]: https://github.com/ruricolist/overlord


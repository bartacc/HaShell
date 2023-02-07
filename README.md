# HaShell

This is a unix shell written in haskell. 

## Running the project

1. Install [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/)
2. Run `stack run`

## Supported features
- Run external command (e.g. `ls -la`)
- Run builtin command. One of:
  - `jobs` - prints info about jobs started by this shell
  - `kill [job number]` - terminates the given job by sending SIGTERM signal to it
  - `bg [job number]` - continues currently suspended job in background
  - `fg [job number]` - continues job and moves it to foreground
  - `cd [new directory]` - changes current directory
  - `quit` - exits the shell
- Run command in background (e.g. `sleep 1000 &`)
- Redirect input or output to file (e.g. `ls -la > test.txt` or `cat < test.txt`)
- Combine commands with a pipeline (e.g. `ls -la | wc -l`)


## Some interesting design decisions specific to functional programming

### State
Since haskell is purely functional, we can't have one, global state. That's why all stateful code is inside a State monad (or to be specific in a `StateT JobsState IO ()` state monad transformer combined with IO monad). This makes it very easy to instantly tell which functions will modify state.

### Separation of pure and monadic functions
The `JobsState.hs` file contains only pure functions, which given current state and some parameters, produce new state. This makes it very easy to test logic of updating state in separation. Then code inside State monad uses these functions to orchestrate update operations.

### Concurrency with STM
While most of the shell concurrency uses the OS directly (fork and execve to create a new unix process), there is one place where Haskell's multithreading had to be used. 

- When shell receives SIGCHLD signal (meaning that a child process has changed its state), GHC runs the `Jobs.sigchldHandler` function in a new Haskell thread
- That handler gathers info about all children that changed state and sends it atomically over an STM (Software Transactional Memory) channel (bascially a FIFO queue).
- Later main thread reads atomically from that channel (within an IO monad), whenever it wants to have the most up to date state. See `StmChannelCommunication.updateStateFromChannelBlocking` and `StmChannelCommunication.updateStateFromChannelNonBlocking`.

Nice thing about STM is that it guarantees that each STM computation will be executed atomically without using locks. If there is more than one computation running at the same time, each one executes, but only one of them commits the changes it made. All the other computations are simply restarted, until they are able to commit their changes (that is there was no conflict).
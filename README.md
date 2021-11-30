# Docker Desktop TUI
A text UI that mimics docker desktop.\
Members(github usernames): hcyang99, juliajyh
## GUI Specs
Idea originates from https://github.com/skanehira/docui with an implementation by Go.

Functions are as follows:
 - image

    - search/pull/remove
    - save/import/load
 
 - container
    - create/remove
    - start/stop/kill
    - export/commit
 
 The work flow is explained by the following images:
 
 **Pull a new image**:
 ![](docs/specs/pull-image.png)

 **Create and configure a new container within the image**
 ![](docs/specs/new-container.png)
 
 **run commands inside container**
 ![](docs/specs/exec.png)
 ![](docs/specs/exec-2.png)
 
 **All actions so far displayed on the Tasks Column**
 ![](docs/specs/tasks.png)
 
 **Result of the bash commands reflected in the status of the new row in container list column**
 ![](docs/specs/ps.png)
 
 **Remove Container**
 ![](docs/specs/rm-prompt.png)

## Architecture
### Front End
UI code that displays the tui in the tty. Using the Brick library.

### Back End
Executes the docker shell commands using the System.Process library.\
Parses the output from docker shell using the Text.Parsec library.

## Milestone::Challenges
- Could not build Brick demos following the instructions --> Learned to use stack
- Haskell language server continued to crash --> Manually installed the previous version

## Milestone::Progress
- The back-end parser working
- Working on front-end and executing docker shell cmd in Haskell
- Expected to finish on time
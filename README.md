# Docker TUI

## A User Interface allowing operations on Docker Components

Members(github usernames): hcyang99, juliajyh

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
 ![Pull new image](https://github.com/juliajyh/cse230-finalproject/blob/main/requirements/Screen%20Shot%202021-11-11%20at%2010.49.08%20AM.png)

 **Create and configure a new container within the image**
 ![](https://github.com/juliajyh/cse230-finalproject/blob/main/requirements/Screen%20Shot%202021-11-11%20at%2010.50.08%20AM.png)
 
 **run commands inside container**
 ![](https://github.com/juliajyh/cse230-finalproject/blob/main/requirements/Screen%20Shot%202021-11-11%20at%2010.50.44%20AM.png)
 ![](https://github.com/juliajyh/cse230-finalproject/blob/main/requirements/Screen%20Shot%202021-11-11%20at%2010.58.00%20AM.png)
 
 **All actions so far displayed on the Tasks Column**
 ![](https://github.com/juliajyh/cse230-finalproject/blob/main/requirements/Screen%20Shot%202021-11-11%20at%2010.51.09%20AM.png)
 
 **Result of the bash commands reflected in the status of the new row in container list column**
 ![](https://github.com/juliajyh/cse230-finalproject/blob/main/requirements/Screen%20Shot%202021-11-11%20at%2010.58.33%20AM.png)
 
 **Remove Container**
 ![](https://github.com/juliajyh/cse230-finalproject/blob/main/requirements/Screen%20Shot%202021-11-11%20at%2010.59.33%20AM.png)
 
 **Successful Execution displayed in Tasks Column**
 ![](https://github.com/juliajyh/cse230-finalproject/blob/main/requirements/Screen%20Shot%202021-11-11%20at%2011.00.07%20AM.png)
 
 **The container named go was removed from container list**
 ![](https://github.com/juliajyh/cse230-finalproject/blob/main/requirements/Screen%20Shot%202021-11-11%20at%2011.00.14%20AM.png)

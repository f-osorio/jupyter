A mismash of files related to Jupyter/Binderhub

# Trying Jupterhub and Binderhub

1. [JupyterHub](#installation-of-jupyterhub)
2. [BinderHUb](#installation-of-binderhub)

# Installing JupyterHub:

## with Minikube (local)
1. [Follow these instructions](https://www.studytrails.com/devops/kubernetes/install-minikube-and-docker-with-virtualbox-on-windows-10-home/)
    * Run `Docker Quickstart Terminal` as _administrator_
    * __IMPORTANT__ make sure to start the virtual machine with adequate memory and processing:
        * `minikube start --memory=<xxxmb> --cpus=<x> --vm-driver=virtualbox`
        * memory = 4096
        * cpus = 4
2. Get [Helm](https://github.com/kubernetes/helm/releases) version > 3
    * Version 3+ no longer has `tiller` which is part of the instructions for JupyterHub
3. Add HELM to the PATH
    * Might need to restart `Docker Quickstart Terminal` afterward
4. Follow [these instructions](https://zero-to-jupyterhub.readthedocs.io/en/latest/setup-jupyterhub/setup-helm.html) to set up HELM
5. [Setup JupyterHub](https://zero-to-jupyterhub.readthedocs.io/en/latest/setup-jupyterhub/setup-jupyterhub.html)
    * Running this locally means there is no `LoadBalancer` so there will not be an external IP address. To access the JupyterHub:
        * the command `kubemini tunnel` will create an external IP address that can be used to access the service. The `minikube` might need to be restarted after this before the IP will work.
        * use `kubectl get service --namespace jhub` to list the running services. _proxy-public_ should have an "EXTERNAL-IP" address

### Setting up existing Docker Image for Jupyter
There are a [number of Docker images](https://jupyter-docker-stacks.readthedocs.io/en/latest/using/selecting.html) ready to be deployed with JupyterHub. Some are very basic and some provide support for multiple languages with different packages. To make one of them default:
* To `config.yaml` add
```
singleuser:
  image:
    name: <image-name>
    tag: <image-tag>
```
[instructions](https://zero-to-jupyterhub.readthedocs.io/en/latest/customizing/user-environment.html#choose-and-use-an-existing-docker-image)

#### [Multiple Profiles](https://zero-to-jupyterhub.readthedocs.io/en/latest/customizing/user-environment.html#using-multiple-profiles-to-let-users-select-their-environment)
There is support to setup multiple profiles which would allow users to select from different docker images and even hardware or interface (default, JupyterLab, RStudio)

### Providing Files to All Users
If there are files that all users should have access to,
1. Add files to user storage:
    * this involves copying files from one directory to another
    * the draw back for this is that it will run everytime the server starts
2. `nbgitpuller`
    * is used to synchornize a folder in a user's filesystem with a git repository
    * this also happens whenever the user starts their server
    * recommended method

### Database
Default is a sqlite database stored on "a persistant volume attached to the hub." It is also possible to use an external RDBMS database.

### Errors & Debugging
_500_ error or event log keeps saying "Server requested," check _hub_ log if it says,  
> TypeError: '<' not supported between instances of 'NoneType' and 'datetime.datetime'
* deploy patch [here](https://github.com/jupyterhub/kubespawner/issues/354)

_timeout_ error when upgrading through `helm`:
* Check events: `kubectl get events -n jhub`

If after restarting `minikube` the hub won't load, try removing the tunnel and recreating it, or if there are any conflicting routes when using `minikube tunnel`:
* `minikube tunnel -c true`
* `minikube tunnel`

Useful commands:
* Check logs:
    * `kubectl --namespace=<namespace> get pod` -> find the pod's nane
    * `kubectl --namespace=<namespace> logs <pod_name>`
* `kubectl --namespace=<namespace> describe pod <pod_name>` -> details about the state of a pod, lists "events" at the bottom. This is the most likely place to find an error
* `kubectl describe nodes`
* `helm list` -> will give namespace, should be 'jhub' following the above instructions
* `kubectl get events`


# Installing BinderHub:

## with Minikube (local)
1. [Follow these instructions](https://www.studytrails.com/devops/kubernetes/install-minikube-and-docker-with-virtualbox-on-windows-10-home/)
    * Run `Docker Quickstart Terminal` as _administrator_
    * __IMPORTANT__ make sure to start the virtual machine with adequate memory and processing:
        * `minikube start --memory=<xxxmb> --cpus=<x> --vm-driver=virtualbox`
        * memory = 4096
        * cpus = 4
2. Get [Helm](https://github.com/kubernetes/helm/releases) version > 3
    * Version 3+ no longer has `tiller` which is part of the instructions for JupyterHub
3. Add HELM to the PATH
    * Might need to restart `Docker Quickstart Terminal` afterward
4. Follow [these instructions](https://zero-to-jupyterhub.readthedocs.io/en/latest/setup-jupyterhub/setup-helm.html) to set up HELM
5. [Create a Docker registry server](https://binderhub.readthedocs.io/en/latest/setup-registry.html)
    * it is possible to use a custom registry, but it requires more work
6. [Follow instructions](https://binderhub.readthedocs.io/en/latest/setup-binderhub.html) to install BinderHub
    * when installing the `helm` check that you are using the most recent [version](https://jupyterhub.github.io/helm-chart/#development-releases-binderhub)
    * if there's a "internal server error" when trying to launch, might need to enable `CORS`:
        * to `secret.yaml` add:
        ```
        cors: &cors
          allowOrigin: '*'
        ```

### Debugging
* `Error: could not find tiller` - Make sure Helm was setup (step 4 above)
* Use `minikube tunnel` to create an externally accessible IP (see JupyterHub instructions for details)
* Not showing build progress (Networking issue?)
    * Messages appear right before the notebook is launched
    * The eventstream is being held up until everything is finished?
    * buffering issue somewhere in the internal network?
    * https://github.com/jupyterhub/binderhub/issues/487
    * __have not found a solution__
* Turn it off an on again:
    * `minikube stop`
    * `minikube start`


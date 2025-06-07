# Executable Environment for OSF Project [4a9b6](https://osf.io/4a9b6/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** Third-Party Social Interaction Targets Enhance Infants' Visual Learning

**Project Description:**
> In this project we aimed to investigate whether 13-month-old infants selectively learn cue-target associations guiding them to videos depicting a social interaction scene. In a visual learning task, two geometrical shapes were repeatedly paired with two kinds of target videos: two adults interacting with one another (social interaction condition), or the same adults acting individually (non-interactive control condition). We found that infants’ saccadic latencies toward the cued target region were faster during social interaction trials compared to non-interactive control trials. In addition, infants performed more predictive gaze shifts in the social interaction condition. 

**Original OSF Page:** [https://osf.io/4a9b6/](https://osf.io/4a9b6/)

---

**Important Note:** The contents of the `4a9b6_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## flowR Integration

This version of the repository has the **[flowR Addin](https://github.com/flowr-analysis/rstudio-addin-flowr)** preinstalled. flowR allows visual design and execution of data analysis workflows within RStudio, supporting better reproducibility and modular analysis pipelines.

To use flowR, open the project in RStudio and go to `Addins` > `flowR`.

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_4a9b6-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_4a9b6-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `4a9b6_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

### Pull the Docker Image

```bash
docker pull meet261/repo2docker-4a9b6-f:latest
```

### Launch RStudio Server

Run the container (with a name, e.g. `rstudio-dev`):
```bash
docker run -it --name rstudio-dev --platform linux/amd64 -p 8888:8787 --user root meet261/repo2docker-4a9b6-f bash
```

Inside the container, start RStudio Server with no authentication:
```bash
/usr/lib/rstudio-server/bin/rserver --www-port 8787 --auth-none=1
```

Then, open your browser and go to: [http://localhost:8888](http://localhost:8888)

> **Note:** If you're running the container on a remote server (e.g., via SSH), replace `localhost` with your server's IP address.
> For example: `http://<your-server-ip>:8888`

## Looking for the Base Version?

For the original Binder-ready repository **without flowR**, visit:
[osf_4a9b6](https://github.com/code-inspect-binder/osf_4a9b6)


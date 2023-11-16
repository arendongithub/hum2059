FROM ghcr.io/maastrichtu-ids/jupyterlab:latest
# Change to root user to install packages requiring admin privileges:
USER root
RUN apt-get update && \
    apt-get install -y vim
RUN fix-permissions /home/$NB_USER
# Switch back to the notebook user for other packages:
USER ${NB_UID}
RUN mamba install -c defaults -y rstudio
RUN pip install jupyter-rsession-proxy
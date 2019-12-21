############################################################
# Dockerfile to build Owl-Symbolic image
# Based on owlbarn/owl_symbolic master branch
# By Liang Wang <liang.wang@cl.cam.ac.uk>
############################################################

FROM owlbarn/owl:latest
USER opam

ENV OWLSYMPATH /home/opam/owl-symbolic
COPY . ${OWLSYMPATH}
RUN cd ${OWLSYMPATH} && opam pin .

WORKDIR ${OWLSYMPATH}
ENTRYPOINT [ "/bin/bash" ]
FROM clfoundation/sbcl:2.2.2-slim
ARG QUICKLISP_DIST_VERSION=2022-02-20


ADD https://beta.quicklisp.org/quicklisp.lisp /root/quicklisp.lisp

RUN set -x; \
  sbcl --load /root/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(ql:uninstall-dist "quicklisp")' \
    --eval "(ql-dist:install-dist \"http://beta.quicklisp.org/dist/quicklisp/${QUICKLISP_DIST_VERSION}/distinfo.txt\" :prompt nil)" \
    --quit && \
  echo '#-quicklisp (load #P"/root/quicklisp/setup.lisp")' > /root/.sbclrc && \
  rm /root/quicklisp.lisp

WORKDIR /app
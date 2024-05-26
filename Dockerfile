FROM debian:bookworm

WORKDIR /
RUN apt update
RUN apt -y install git curl sbcl libglfw3 libglx0 libzstd-dev
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp \
         --eval "(quicklisp-quickstart:install)" \
	 --eval "(ql:quickload :deploy)" \
         --quit
RUN echo '(load "/root/quicklisp/setup.lisp")' >> /root/.sbclrc

# RUN git clone https://github.com/NoamZeise/gficl.git
WORKDIR /gficl
COPY . .
RUN sbcl --load "gficl-examples.asd" \
         --eval "(ql:quickload :gficl-examples)" \
	 --eval "(asdf:make :gficl-examples)"

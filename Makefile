LISP="sbcl"
build:
	$(LISP) --load "gficl-examples.asd" \
                --eval "(ql:quickload :gficl-examples)" \
                --eval "(asdf:make :gficl-examples)"

asdf: # build without quicklisp 
	$(LISP) --load "gficl-examples.asd" \
                --eval "(asdf:load-system :gficl-examples)" \
                --eval "(asdf:make :gficl-examples)"

clean:
	rm -r bin

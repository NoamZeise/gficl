LISP="sbcl"
build:
	$(LISP) --eval "(ql:quickload :deploy)" \
		--load "gficl.asd" \
		--load "gficl-examples.asd" \
		--eval "(ql:quickload :deploy)" \
                --eval "(ql:quickload :gficl-examples)" \
                --eval "(asdf:make :gficl-examples)"

asdf: # build without quicklisp 
	$(LISP) --load "gficl.asd" \
		--load "gficl-examples.asd" \
                --eval "(asdf:load-system :gficl-examples)" \
                --eval "(asdf:make :gficl-examples)"

clean:
	rm -r bin

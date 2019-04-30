# Proto-Cl-Client-Side-Rendering - A prototype of hot loads in Common Lisp using WebSocket

Proto-Cl-Client-Side-Rendering is a prototype of client side rendering in Common Lisp.

## Execute sample

Please clone this (and [ps-experiment](https://github.com/eshamster/ps-experiment)) to where quicklisp (asdf) can find. Then, load it by `ql:quickload` and start server. After that, you can access to http://localhost:5000 .

```lisp
CL-USER> (ql:quickload :sample-proto-cl-client-side-rendering)
CL-USER> (sample-proto-cl-client-side-rendering:start :port 5000)
```


## Author

* eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2019 eshamster (hamgoostar@gmail.com)

## License

Licensed under the MIT License.

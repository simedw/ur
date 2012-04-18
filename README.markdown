UR - undefined reference
========================
Just a small project made for easily locating omitted libraries at link time.

Usage
=====

The first the ur is used, the database needs to be rebuild

    % ur -r

This will take a few minutes.

This is a normal usage scenario:

    % cat test.c
      #include <math.h>

      int main(){
        long double tmp = tanl(23);
        return 0;
     }
    % clang test.c
      /tmp/test-JmcoIs.o:test.c:function main: error: undefined reference to 'tanl'
    % clang test.c 2>&1 | ur
        -lm

To automatically rerun clang or gcc with the missing flags:

    % ur -- clang test.c
        rerunning with missing flag : -lm
Build
=====

    % cabal configure
    % cabal build
    % cabal install



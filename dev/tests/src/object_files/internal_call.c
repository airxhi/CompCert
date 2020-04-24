int foo(int a){
    return a+1;
}

int bar(int a){
    return a + foo(a);
}
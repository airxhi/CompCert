
int fun(int a){
    return a;
}

int main(){
    int (*fun_ptr)(int) = &fun;

    (*fun_ptr)(10);

    return 0;
}
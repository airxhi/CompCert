int foo(){
    int total = 0;
    for (int i = 0; i < 10; i++){
        total = total + i;
    }
    return total;
}

int main(){
    int x  = foo();
    return 0;
}
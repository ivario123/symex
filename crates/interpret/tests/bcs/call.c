int foo(int a) {
    if (a < 0) {
        return -1;
    } else {
        return 1;
    }
}

int bar(int a) {
    return foo(a);
}

int main() {
    return bar(0);
}

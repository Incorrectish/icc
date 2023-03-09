int add(int a, int b) {
    return a + b;
}

int hi(int a) {
    return a;
}


int main() {
    int a = 7;
    for(int b = 0; b < 10; b++) {
        int i;
        for (i = 0; i < 10; ++i) {
            a++;
        }
    }
    return a;
}

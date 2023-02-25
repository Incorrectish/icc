int main() {
    int a = 7;
    for (int i = 0; i < 5; i++) {
        if (i == 3) {
            a += 4;    
            continue;
        }
        a += 2;
    }
}

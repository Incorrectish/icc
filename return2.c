int main() {
    int j = 5;
    for (int i = 0; i < 10; i++) {
        j++;
        if (j > 10) {
            break;
        }
    }
    return j;
}

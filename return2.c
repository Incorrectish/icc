int sum(int a, int b) {
    return a + b;
}

int main() {
    int a = sum(1, 2) - (sum(1, 2) / 2) * 2;
    return a;
}

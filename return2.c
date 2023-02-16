int main() {
    int a = 5, b, c = 7;
    b = ((a += 3) % c );
    return b % 5;
}

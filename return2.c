int main() {
    int a = 7;
    {
        a += 7;
        int b = 8;
    }
    return a;
}

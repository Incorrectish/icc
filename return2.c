int main() {
    int a = 7;
    {
        int a = 3;
        {
            a += 1;
        }
    }
    {
        a += 2;
    }
    int a = 5;
    return a;
}

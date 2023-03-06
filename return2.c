int main() {
    int i = 0;
    {
        {
            i += 9;
            int i = 0;
        }
    }
    {
        int i = 1;
        i++;
    }
    return i;
}

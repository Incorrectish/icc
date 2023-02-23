int main() {
    int a = 7;
    if (1) {
        {
            a += 1;
        }
        int c = 3;
    } else {
        return 2;
    }

    {
        {
            int a = 1;
        }
        {
            {
                {
                    {
                        {
                            {
                                int c = 3;
                            }
                        }
                    }
                }
            }
        }
        int b = 5;
        b += 1;
        {
            {
                {
                    int a = 5;
                }
            }
        }
    }
    return a > 5? a-- : ++a;
}

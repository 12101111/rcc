int ans = 1 + 2;
void test(int ans2);
int test2(int ans);
int main(void)
{
    int ans2 = 3 * 2 + 1;
    ans2 = ans;
    test(ans2);
    if (ans2 == 7)
    {
        return (-1);
    }
    else if (ans2 == 8)
    {
        return (-2);
    }
    else
    {
        while (ans2 >= 0)
        {
            ans2 = ans2 - 1;
        }
        return test2(ans2);
    }
}

void test(int ans2)
{
    ans2 = 8;
}

int test2(int ans)
{
    return ans + 1;
}
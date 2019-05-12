int global = 7;   //全局变量
void test(int a); //形参
int test2(int i); //返回值
int main(void)
{
    int alpha = 1 + 3 * 2; //运算符优先级
    test(alpha);           //形参/实参
    if (alpha == 8)
    {
        return 1 + -(1 + 1); //单目运算符
    }
    else if (alpha != global)//读全局变量
    {
        return -2;
    }
    else
    {
        do
        {
            alpha = alpha + 1;
        } while (alpha < 10);
        while (alpha >= alpha - alpha) //>=优先级
        {
            alpha = alpha - 1;
        }
        return test2(alpha); //函数返回值
    }
    return -3;//不可达
}
void test(int a)
{
    a = 8;
}
int test2(int i)
{
    return i + 1;
}

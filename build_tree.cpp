#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
#define rep(i,a,b) for(int i = (a);i <= (b);++i)
#define re_(i,a,b) for(int i = (a);i < (b);++i)
#define dwn(i,a,b) for(int i = (a);i >= (b);--i)

const int SZ = 1e6 + 3;

char s[SZ];

template<typename Type>inline void read(Type &xx){
    Type f = 1;char ch;xx = 0;
    for(ch = getchar();ch < '0' || ch > '9';ch = getchar()) if(ch == '-') f = -1;
    for(;ch >= '0' && ch <= '9';ch = getchar()) xx = xx * 10 + ch - '0';
    xx *= f;
}

bool isAllNumber(string s){
    if(s[0] == '-' && s.size() == 1) return false;
    for(unsigned i = (s[0] == '-');i < s.size();++i) if(!isdigit(s[i])) return false;
    return true;
}

struct Val{
    int typ;//0=整数，1=符号，2=变量
    int ival;char cval;string var;
    Val():typ(-1){}
    Val(int ival):typ(0),ival(ival){}
    Val(char cval):typ(1),cval(cval){}
    Val(string var):typ(2),var(var){}
    void disp(){
        if(typ == 0) cout << ival;
        else if(typ == 1) cout << cval;
        else cout << var;
    }
};

struct AST{
    Val val;
    vector<AST*> son;
    AST(){}
    AST(Val v):val(v){}
    
    void dispThis(AST* u){//输出节点u信息
        cout << "Node type: " << u->val.typ << " ";
        u->val.disp();
        puts("");
    }
    void disp(){//输出自己信息
        dispThis(this);
    }
    void print(AST* u){//遍历u子树
        dispThis(u);
        for(auto v: u->son){
            print(v);
        }
    }
    void dfs(){//遍历自身的子树
        print(this);
    }
};

class Parser{
private:
    char s[SZ];int sl;
    Val cur;int pos;//解析表达式的需求下，指针不用回退，因此设为全局变量
public:
    AST *rt;
    Parser(char *s):sl(0),pos(0){
        for(;*s != '\0' && *s != '\n';++s){//去除无关符号;'^'表示求幂
            if(!isalpha(*s) && !isdigit(*s) && *s != '_' &&
                *s != '+' && *s != '-' && *s != '*' && *s != '/' &&
                *s != '^' && *s != '(' && *s != ')') continue;
            this->s[sl++] = *s;
        }
        rt = NULL;
    }
    void advance(){
        if(pos >= sl) return;
        if(isdigit(s[pos])){
            int x = 0;
            for(;pos < sl && isdigit(s[pos]);)
                x = 10 * x + s[pos++] - '0';
            cur = Val(x);
        }
        else if(isalpha(s[pos]) || s[pos] == '_'){
            string x;
            for(;pos < sl &&
                (isalpha(s[pos]) || isdigit(s[pos]) ||
                s[pos] == '_');) x += s[pos++];
            cur = Val(x);
        }
        else cur = Val(s[pos++]);
    }
    
    void parse(){
        rt = expression();
    }
    AST* expression(){
        AST *u = mul();
        while(pos < sl && s[pos] == '+' || s[pos] == '-'){
            AST *rt = new AST(s[pos]);
            rt->son.push_back(u);
            advance();//跳过运算符
            AST *right = mul();
            rt->son.push_back(right);
            u = rt;
        }
        return u;
    }
    AST* mul(){
        advance();
        AST *u = new AST(cur);
        while(pos < sl && s[pos] == '*' || s[pos] == '/'){
            AST *rt = new AST(s[pos]);
            rt->son.push_back(u);
            advance();//跳过运算符
            advance();
            rt->son.push_back(new AST(cur));
            u = rt;
        }
        return u;
    }
};

struct Simplifier{
    AST *rt;
    string res;
    Simplifier(AST *rt):rt(rt){}
    bool hasNoAM(AST *u){//没有加or减号节点
        if(u->val.typ == 1){
            if(u->val.cval == '+' || u->val.cval == '-')
                return false;
        }
        for(auto v: u->son){
            if(!hasNoAM(v)) return false;
        }
        return true;
    }
    string simplify(){
        return res = dfs(rt);
    }
    string dfs(AST *u){
        string res;
        if(u->val.typ == 0) return to_string(u->val.ival);
        else if(u->val.typ == 2) return u->val.var;
        if(u->val.cval == '+'){
            res = dfsAddSon(u);
        }
        else if(u->val.cval == '-'){
            res = dfsMinusSon(u);
        }
        else if(u->val.cval == '*'){
            res = dfsMulSon(u);
        }
        else if(u->val.cval == '/'){
            res = dfsDivSon(u);
        }
        else{
            res = dfsPowSon(u);
        }
        return res;
    }
    string dfsAddSon(AST *u){
        string res = dfs(u->son[0]);
        vector<int> digits;
        for(unsigned i = 1;i < u->son.size();++i){
            AST *v = u->son[i];
            string subRes = dfs(v);
            if(subRes == "0") continue;
            if(isAllNumber(subRes)){
                digits.push_back(atoi(subRes.c_str()));
                continue;
            }
            res += "+" + subRes;
        }
        int sum = 0;
        for(auto d: digits) sum += d;
        if(sum != 0){
            if(isAllNumber(res)){
                res = to_string(atoi(res.c_str()) + sum);
            }
            else res += "+" + to_string(sum);
        }
        return res;
    }
    string dfsMinusSon(AST *u){
        string res = dfs(u->son[0]);
        vector<int> digits;
        for(unsigned i = 1;i < u->son.size();++i){
            AST *v = u->son[i];
            string subRes = dfs(v);
            if(subRes == "0") continue;
            if(isAllNumber(subRes)){
                digits.push_back(atoi(subRes.c_str()));
                continue;
            }
            res += "-";
            if(!hasNoAM(v)) subRes = "(" + subRes + ")";
            res += subRes;
        }
        int sum = 0;
        for(auto d: digits) sum += d;
        if(sum != 0){
            if(isAllNumber(res)){
                res = to_string(atoi(res.c_str()) - sum);
            }
            else res += "-" + to_string(sum);
        }
        return res;
    }
    string dfsMulSon(AST *u){
        string res = dfs(u->son[0]);
        if(res == "0") return res;
        vector<int> digits;
        for(unsigned i = 1;i < u->son.size();++i){
            AST *v = u->son[i];
            string subRes = dfs(v);
            if(subRes == "1") continue;
            if(isAllNumber(subRes)){
                digits.push_back(atoi(subRes.c_str()));
                continue;
            }
            res += "*";
            if(!hasNoAM(v)) subRes = "(" + subRes + ")";
            res += subRes;
        }
        int tot = 1;
        for(auto d: digits) tot *= d;
        if(tot == 0) return "0";
        if(tot != 1){
            if(isAllNumber(res)){
                res = to_string(atoi(res.c_str()) * tot);
            }
            else res += "*" + to_string(tot);
        }
        return res;
    }
    string dfsDivSon(AST *u){
        string res = dfs(u->son[0]);
        for(unsigned i = 1;i < u->son.size();++i){
            AST *v = u->son[i];
            string subRes = dfs(v);
            if(subRes == "1") continue;
            res += "/";
            res += "(" + subRes + ")";
        }
        return res;
    }
    string dfsPowSon(AST *u){
        string res = dfs(u->son[0]);
        for(unsigned i = 1;i < u->son.size();++i){
            AST *v = u->son[i];
            string subRes = dfs(v);
            res += "^";
            res += "(" + subRes + ")";
        }
        return res;
    }
};

int main(int argc, char** argv) {
    while(true){
        fgets(s,SZ,stdin);
        Parser *pr = new Parser(s);
        pr->parse();
        pr->rt->dfs();//
        Simplifier sr(pr->rt);
        cout << sr.simplify() << endl;
    }
    return 0;
}
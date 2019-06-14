#include<iostream>
#include <cmath>
#include <vector>

class Bond
{
    public:
        Bond(double F, double issue_date, int num_periods, int freq,
        const std::vector<double> &c);//written
        ~Bond();//written
        void setFlatCoupons(double c);//written
        void setCoupons(const std::vector<double> &c);//written
        double FairValue(double t0, double y) const;//written
        double maturity() const { return _maturity; }
        double issue() const { return _issue; }
        int FV_duration(double t0, double y, double &B,
        double &Mac_dur, double &mod_dur) const; //written
    private:
        double _Face;//init
        double _issue;//init
        double _maturity;//init
        int _cpnFreq;//init
        int _numCpnPeriods;//init
        std::vector<double> _cpnAmt;//init
        std::vector<double> _cpnDate;//init
};

Bond::Bond(double F, double issue_date, int num_periods, int freq,const std::vector<double> &c)
{
    _Face = F;
    if(F < 0)
    {
        std::cout<<"The Face Value is below 0 please check your Inputs The Class will Default to 0"<<std::endl;
        _Face = 0;
    }

    _cpnFreq = freq;
    if(freq < 1)
    {
        std::cout<<"The Frequency is below 1 The Class will default to 1"<<std::endl;
        _cpnFreq = freq;
    }

    _numCpnPeriods = num_periods;
    if(num_periods < 1)
    {
        std::cout<<"The number of Periods is below 1 The Class will default to 1"<<std::endl;
        _numCpnPeriods = 1;
    }    
    
    _issue = issue_date;

    _maturity = _issue + _numCpnPeriods/_cpnFreq;

    _cpnAmt.resize(_numCpnPeriods);
    _cpnDate.resize(_numCpnPeriods);

    for(double i = 0; i < _numCpnPeriods; ++i)
    {
        _cpnDate[i] = _issue + (i+1)/freq;

        //std::cout<<"the "<<i<<"th date is: "<<_cpnDate[i]<<std::endl;
    }

    //std::cout<<"maturity date is: "<<_maturity<<std::endl;
    //std::cout<<"last coupon date is: "<<_cpnDate.back()<<std::endl;

    if(c.size() == 1)
        setFlatCoupons(c[0]);
    else
        setCoupons(c);
}

Bond::~Bond(){}

void Bond::setFlatCoupons(double c)
{
    if (c < 0.0) 
        c = 0.0;
    std::fill(_cpnAmt.begin(), _cpnAmt.end(), c);

    // for(int i = 0; i < _numCpnPeriods; ++i)
    // {
    //     std::cout<<"coupon at date: "<< i <<" is: "<<_cpnAmt[i]<<std::endl;
    // }
}

void Bond::setCoupons(const std::vector<double> &c)
{
    for(int i = 0; i < _numCpnPeriods; ++i)
    {
        if(c.size() <= i)
        {
            if(c.back() < 0)
                _cpnAmt[i] = 0;
            else
                _cpnAmt[i] = c.back();
        }
        else
        {
            if(c[i] < 0)
                _cpnAmt[i] = 0;
            else
                _cpnAmt[i] = c[i];
        }
    }


}
int Bond::FV_duration(double t0, double y, double &B,double &Mac_dur, double &mod_dur) const
{
    //init Vars
    B = 0;
    Mac_dur = 0;
    mod_dur = 0;
    if(t0 < _issue || t0 >= _maturity)
        return -1; //incorrect inputs
    const double tol = 1.0e-6;
    const double y_d = 0.01 * y;
    const double denom_pranth = 1 + (y_d/_cpnFreq);
    
    for(int i = 0; i < _numCpnPeriods; ++i)
    {
        double t_i = _cpnDate[i];
        double t_f = t_i - t0;
        //std::cout<<"t_i is: "<<t_i <<" t_0  + tol is: "<<(t0 + tol)<<std::endl;
        if(t_i >= (t0 + tol))
        {
            double denom = pow(denom_pranth,_cpnFreq*(t_f));
            if( i != (_numCpnPeriods-1))
            {
                B += (_cpnAmt[i]/_cpnFreq)/denom;
                //std::cout<<"B at itter: "<<i <<" is: "<<B<<std::endl;
                Mac_dur += (t_f) * ((_cpnAmt[i]/_cpnFreq)/denom);
            }
            else
            {
                B += (_Face + (_cpnAmt[i]/_cpnFreq))/denom;
                // std::cout<<"Num at "<<i <<" is: "<<(_Face + (_cpnAmt[i]/_cpnFreq))<<std::endl;
                // std::cout<<"denom at "<<i <<" is: "<<denom<<std::endl;
                // std::cout<<"denom praeanth "<<i <<" is: "<<denom_pranth<<std::endl;
                // std::cout<<"B at itter: "<<i <<" is: "<<B<<std::endl;
                Mac_dur += (t_f) * ((_Face + (_cpnAmt[i]/_cpnFreq))/denom);
                if(B == 0)
                    return -2;
                Mac_dur = Mac_dur * pow(B, -1);
                
            }
        }

        mod_dur = Mac_dur/denom_pranth;

    }


    return 0;
    

}

double Bond::FairValue(double t0, double y) const
{
    double B = 0;
    double dummy1 = 0;
    double dummy2 = 0;
    int j = FV_duration(t0, y, B, dummy1, dummy2);
    if(j == -1)
    {
        std::cout<<"incorrect t_0 "<<std::endl;
        return -1;
    }
    if(j == -2)
    {
        std::cout<<"B is 0 cant Div by zero for Maculay "<<std::endl;
        return -1;
    }
    return B;
}
int yield(double &y, int &num_iter, const Bond &bond, double B_target, double t0,double tol=1.0e-4, int max_iter=100)
{
    y = 0;
    num_iter = 0;
    if(B_target <= 0 || t0 <bond.issue() || t0 > bond.maturity())
        return 1;
    double y_low = 0.0;
    double y_high = 100.0;
    double Bylow = bond.FairValue(t0, y_low);
    if(Bylow < 0)
        return 1;
    if(Bylow == -1)
        return 1;
    double diffBylow = Bylow - B_target;
    if(std::abs(diffBylow) <= tol)
    {
        y =y_low;
        return 0;
    }

    double Byhigh = bond.FairValue(t0, y_high);
    if(Byhigh < 0)
        return 1;
    double diffByhigh = Byhigh - B_target;

    if(std::abs(diffByhigh) <= tol)
    {
        y = y_high;
        return 0;
    }

    // Failure due to incorret bracket
    if((diffBylow * diffByhigh) > 0)
    {
        y = 0;
        return 1;
    }
    
    for (num_iter = 1; num_iter < max_iter; ++num_iter)
    {
        y = (y_low + y_high)/2;
        double B = bond.FairValue(t0, y);
        if(B < 0)
            return 1;
        double diffB = B - B_target;
        if(std::abs(diffB) <= tol)
            return 0;
        if(diffB * diffBylow > 0.0)
            y_low = y;
        else
            y_high = y;
        if(std::abs(y_high - y_low) <= tol)
            return 0;
    }

    y = 0;
    return 1;
}

//Question 3
int main() {
    std::vector<double> coupons{1,5,1,4,4,6,8,5};
    Bond b(100,0,8,2,coupons);
    double y = 0.0;
    int i = 0;
    int j = yield(y,i,b,113.82,0);
    if(j > 0)
    {
        std::cout<<"Program has Failed"<<std::endl;
        return 0;
    }
    std::cout<<"The yeild is: "<<y<<std::endl;
}
//Fair Valuse HW
// int main() {
//     std::vector<double> coupons{1,5,1,4,4,6,8,5};
//     Bond b(100,0,8,2,coupons);
//     std::cout<<"The Fair Value for y = 0 is: "<<b.FairValue(0.65144685,1.0)<<std::endl;
//     std::cout<<"The Fair Value for y = 2 is: "<<b.FairValue(0.65144685,2.0)<<std::endl;
//     std::cout<<"The Fair Value for y = 4 is: "<<b.FairValue(0.65144685,4.0)<<std::endl;
//     std::cout<<"The Fair Value for y = 6 is: "<<b.FairValue(0.65144685,6.0)<<std::endl;
//     std::cout<<"The Fair Value for y = 8 is: "<<b.FairValue(0.65144685,8.0)<<std::endl;
//     std::cout<<"The Fair Value for y = 10 is: "<<b.FairValue(0.65144685,10.0)<<std::endl;

// }
//Yeild Test
// int main() {
//     std::vector<double> coupons{3};
//     Bond b(100,0,8,2,coupons);
//     double y = 0.0;
//     int i = 0;
//     int j = yield(y,i,b,110,0);
//     if(j > 0)
//     {
//         std::cout<<"Program has Failed"<<std::endl;
//         return 0;
//     }
//     std::cout<<"The yeild is: "<<y<<std::endl;
// }

//Fair Value Test 2
// int main() {
//     std::vector<double> coupons{1};
//     Bond b(100,0,8,2,coupons);
//     std::cout<<"The Fair Value for y = 0 is: "<<b.FairValue(0,0)<<std::endl;
// }


// Fair Value Test 1
// int main() {
//     std::vector<double> coupons{1};
//     Bond b(100,0,8,2,coupons);
//     std::cout<<"The Fair Value for y = 1 is: "<<b.FairValue(0,1)<<std::endl;
// }
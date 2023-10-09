#include <asio/execution.hpp>
#include <asio/static_thread_pool.hpp>
#include <iostream>

using asio::static_thread_pool;
namespace execution = asio::execution;

// Traditional active object pattern.
// Member functions do not block.

class bank_account
{
  int balance_ = 0;
  mutable static_thread_pool pool_{1};

public:
  void deposit(int amount)
  {
    pool_.executor().execute(
        [this, amount]
        {
          balance_ += amount;
        });
  }

  void withdraw(int amount)
  {
    pool_.executor().execute(
        [this, amount]
        {
          if (balance_ >= amount)
            balance_ -= amount;
        });
  }

  void print_balance() const
  {
    pool_.executor().execute(
        [this]
        {
          std::cout << "balance = " << balance_ << "\n";
        });
  }

  ~bank_account()
  {
    pool_.wait();
  }
};

int main()
{
  bank_account acct;
  acct.deposit(20);
  acct.withdraw(10);
  acct.print_balance();
}

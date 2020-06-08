#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

class OscMessageManagerThread : public Thread
{
public:
    OscMessageManagerThread() : Thread("message thread"), ready(false) {
        setPriority(10);
    };

    void run() override
    {
        message_manager = MessageManager::getInstance();
        message_manager->setCurrentThreadAsMessageThread();
        ready = true;
        message_manager->runDispatchLoop();
    }

    ~OscMessageManagerThread()
    {
        MessageManager::deleteInstance();
    }

    void stopDispatchLoop()
    {
        message_manager->stopDispatchLoop();
    }

    bool isReady() {
        return ready;
    }

    template <typename FunctionType>
    bool callAsync(FunctionType functionToCall) {
        return message_manager->callAsync(functionToCall);
    }

private:
    MessageManager* message_manager;
    std::atomic<bool> ready;

};


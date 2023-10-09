#include <ableton/Link.hpp>
#include <driver/gpio.h>
#include <driver/gptimer.h>
#include <esp_event.h>
#include <freertos/FreeRTOS.h>
#include <freertos/semphr.h>
#include <freertos/task.h>
#include <nvs_flash.h>
#include <protocol_examples_common.h>

#define LED GPIO_NUM_2
#define PRINT_LINK_STATE false

unsigned int if_nametoindex(const char* ifName)
{
  return 0;
}

char* if_indextoname(unsigned int ifIndex, char* ifName)
{
  return nullptr;
}

void printTask(void* userParam)
{
  auto link = static_cast<ableton::Link*>(userParam);
  const auto quantum = 4.0;

  while (true)
  {
    const auto sessionState = link->captureAppSessionState();
    const auto numPeers = link->numPeers();
    const auto time = link->clock().micros();
    const auto beats = sessionState.beatAtTime(time, quantum);
    std::cout << std::defaultfloat << "| peers: " << numPeers << " | "
              << "tempo: " << sessionState.tempo() << " | " << std::fixed
              << "beats: " << beats << " |" << std::endl;
    vTaskDelay(800 / portTICK_PERIOD_MS);
  }
}

void tickTask(void* userParam)
{
  ableton::Link link(120.0f);
  link.enable(true);

  if (PRINT_LINK_STATE)
  {
    xTaskCreate(printTask, "print", 8192, &link, 1, nullptr);
  }

  gpio_set_direction(LED, GPIO_MODE_OUTPUT);

  while (true)
  {
    const auto state = link.captureAudioSessionState();
    const auto phase = state.phaseAtTime(link.clock().micros(), 1.);
    gpio_set_level(LED, fmodf(phase, 1.) < 0.1);
    vTaskDelay(1);
  }
}

extern "C" void app_main()
{
  ESP_ERROR_CHECK(nvs_flash_init());
  esp_netif_init();
  ESP_ERROR_CHECK(esp_event_loop_create_default());
  ESP_ERROR_CHECK(example_connect());

  xTaskCreate(tickTask, "tick", 8192, nullptr, configMAX_PRIORITIES - 1, nullptr);
}

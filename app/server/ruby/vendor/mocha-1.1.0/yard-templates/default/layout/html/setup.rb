def init
  super
  if ENV['GOOGLE_ANALYTICS_WEB_PROPERTY_ID']
    sections[:layout] << :google_analytics
  end
end

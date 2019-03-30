package mobi.upod.app.gui;

public enum UsageTipsKey {
  CATEGORY_SELECTION("category_selection"),
  ADD_PODCAST("add_podcast"),
  EPISODE_VIEW_MODE("episode_view_mode"),
  SUBSCRIBE_PODCAST("subscribe_podcast");

  private final String key;

  UsageTipsKey(String key) {
    this.key = key;
  }

  String getKey() {
    return key;
  }
}

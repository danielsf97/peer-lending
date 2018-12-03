import java.time.LocalDateTime;

public class Auction {
    private long value;
    private float maxRate;
    private LocalDateTime startingDateTime;
    private static int duration = 10;

    Auction(long value, float maxRate) {
        this.value = value;
        this.maxRate = maxRate;
        this.startingDateTime = LocalDateTime.now();
    }

    public static int getDuration() {
        return duration;
    }

    public long getValue() {
        return value;
    }

    public float getMaxRate() {
        return maxRate;
    }

    public LocalDateTime getStartingDateTime() {
        return startingDateTime;
    }
}

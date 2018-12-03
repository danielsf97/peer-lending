import java.time.LocalDateTime;

public class Emission {
    private long value;
    private float fixedRate;
    private LocalDateTime startingDateTime;
    private static int duration = 10;

    Emission(long value, float maxRate) {
        this.value = value;
        this.fixedRate = maxRate;
        this.startingDateTime = LocalDateTime.now();
    }

    public static int getDuration() {
        return duration;
    }

    public long getValue() {
        return value;
    }

    public float getMaxRate() {
        return fixedRate;
    }

    public LocalDateTime getStartingDateTime() {
        return startingDateTime;
    }
}

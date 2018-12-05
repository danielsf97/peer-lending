import java.time.LocalDateTime;

public class Emission {
    private long value;
    private float fixedRate;
    private LocalDateTime startingDateTime;
    private static int duration = 10;

    Emission(long value) {
        this.value = value;
        this.startingDateTime = LocalDateTime.now();
    }

    public static int getDuration() {
        return duration;
    }

    public long getValue() {
        return value;
    }

    public float getFixedRate() {
        return fixedRate;
    }

    public LocalDateTime getStartingDateTime() {
        return startingDateTime;
    }
}

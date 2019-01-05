package health;

import com.codahale.metrics.health.HealthCheck;


/**
 * Health check simples (se está a correr está saudável).
 *
 */
public class AppHealthCheck extends HealthCheck {

    public AppHealthCheck() {
        super();
    }

    @Override
    protected Result check() {
        return Result.healthy();
    }
}

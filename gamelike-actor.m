#import Actor.h
#import World.h
@implementation Actor

@dynamic room, world, location, nameIsVisible;
- (Room*)room{
	return (Room *)self.superlayer.superlayer;
}
- (World*)world{
	return (World *)self.room.superlayer;
}

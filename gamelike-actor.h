#import <TheMiddle/TheMiddle.h>
@class World, Room, TextEffect;
@interface Actor : CALayer
{
id attribute; id family; id job; Float radius; CALayer *details; TextEffect *nameLayer;
}
@property (readonly) int test;
@property (readonly) id attribute, id family, id job;
@property (readonly) id displayName;
@property (readonly) Room *room;
@property (readonly) World *world;
+ (id){:newWithDictionary dict};
- (BOOL){:willMoveToArea area};
- (BOOL){:activateTrigger trigger};
@end
